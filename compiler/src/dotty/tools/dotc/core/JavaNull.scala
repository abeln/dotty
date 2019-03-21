package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.JavaDefined
import dotty.tools.dotc.core.StdNames.{jnme, nme}
import dotty.tools.dotc.core.Symbols.{Symbol, defn, _}
import dotty.tools.dotc.core.Types.{AndType, AppliedType, LambdaType, MethodType, OrType, PolyType, Type, TypeAlias, TypeMap, TypeParamRef, TypeRef}

import scala.io.Source._

/** Transformation from Java (nullable) to Scala (non-nullable) types */
object JavaNull {

  // TODO(abeln): do we need this to be multithread-safe?
  private lazy val nullStats: Map[String, ClassStats] = readNullStats()

  /** Adds nullability annotations to a Java-defined member.
   *  `tp` is the member type. The type inside `sym` shouldn't be used (might not be even set).
   */
  def nullifyMember(sym: Symbol, tp: Type)(implicit ctx: Context): Type = {
    assert(ctx.settings.YexplicitNulls.value)
    assert(sym.is(JavaDefined), s"can only nullify java-defined members")

    // A list of members that are special-cased.
    val whitelist: Seq[NullifyPolicy] = Seq(
      // The `TYPE` field in every class.
      FieldP(_.name == nme.TYPE_),
      // The `toString` method.
      MethodP(_.name == nme.toString_, Seq.empty, nnRes = true, nnShallow = true),
      // Constructors: params are nullified, but the result type isn't.
      paramsOnlyP(_.isConstructor),
      // Don't nullify the type of Java enum instances.
      NoOpP(_.is(Flags.JavaEnumValue))
    ) ++ Seq(
      // Methods in `java.lang.String`.
      paramsOnlyP(_.name == nme.split)
    ).map(WithinSym(_, defn.StringClass)) ++ Seq(
      stdLibP(sym, tp)
    )

    val (fromWhitelistTp, handled) = whitelist.foldLeft((tp, false)) {
      case (res@(_, true), _) => res
      case ((_, false), pol) =>
        if (pol.isApplicable(sym)) (pol(tp), true)
        else (tp, false)
    }

    if (handled) {
      fromWhitelistTp
    } else {
      // Default case: nullify everything.
      nullifyType(tp)
    }
  }

  private case class ClassStats(name: String, fields: Seq[FieldStats], methods: Seq[MethodStats]) {
    private def find(sym: Symbol, tp: Type, nameToDesc: Seq[(String, String)])(implicit ctx: Context): Option[Int] = {
      val name = sym.name.show
      sym.descriptor match {
        case Some(desc) =>
          nameToDesc.indexOf((name, desc.mangledString)) match {
            case -1 => None
            case idx => Some(idx)
          }
        case None => None
      }
    }

    def getField(sym: Symbol, tp: Type)(implicit ctx: Context): Option[FieldStats] = {
      find(sym, tp, fields.map(fs => (fs.name, fs.desc))) match {
        case Some(idx) => Some(fields(idx))
        case None => None
      }
    }

    def getMethod(sym: Symbol, tp: Type)(implicit ctx: Context): Option[MethodStats] = {
      find(sym, tp, methods.map(ms => (ms.name, ms.desc))) match {
        case Some(idx) => Some(methods(idx))
        case None => None
      }
    }
  }

  private case class FieldStats(name: String, desc: String, nnTpe: Boolean)
  private case class MethodStats(name: String, desc: String, nnParams: Seq[Int], nnRet: Boolean)

  // TODO(abeln): add better error handling when reading stats
  private def readNullStats(): Map[String, ClassStats] = {
    def text(node: xml.Node, name: String): String = {
      (node \ name).head.text
    }

    def toField(node: xml.Node): FieldStats = {
      FieldStats(
        text(node, "name"),
        text(node, "desc"),
        text(node, "ret").toBoolean)
    }

    def toMethod(node: xml.Node): MethodStats = {
      val params = ((node \ "params").head \ "param").map(p => p.text.toInt)
      MethodStats(
        text(node, "name"),
        text(node, "desc"),
        params,
        text(node, "ret").toBoolean)
    }

    def toClazz(node: xml.Node): (String, ClassStats) = {
      val name = text(node, "name")
      val fields = ((node \ "fields").head \ "field").map(toField)
      val methods = ((node \ "methods").head \ "method").map(toMethod)
      (name, ClassStats(name, fields, methods))
    }

    val stats = scala.xml.XML.loadFile("explicit-nulls-stdlib.xml")
    (stats \ "class").map(toClazz).toMap
  }

  /** A policy that special cases the handling of some symbol or class of symbols. */
  private sealed trait NullifyPolicy {
    /** Whether the policy applies to `sym`. */
    def isApplicable(sym: Symbol): Boolean
    /** Nullifies `tp` according to the policy. Should call `isApplicable` first. */
    def apply(tp: Type): Type
  }

  /** A policy that's never applicable. */
  private case object FalseP extends NullifyPolicy {
    override def isApplicable(sym: Symbol): Boolean = false

    def apply(tp: Type): Type = {
      assert(false, "false nullify policy should never be applied")
      tp
    }
  }

  /** A policy that leaves the passed-in type unchanged. */
  private case class NoOpP(trigger: Symbol => Boolean) extends NullifyPolicy {
    override def isApplicable(sym: Symbol): Boolean = trigger(sym)

    override def apply(tp: Type): Type = tp
  }

  /** A policy that avoids modifying a field. */
  private case class FieldP(trigger: Symbol => Boolean)(implicit ctx: Context) extends NullifyPolicy {
    override def isApplicable(sym: Symbol): Boolean = trigger(sym)
    override def apply(tp: Type): Type = {
      assert(!tp.isJavaMethod, s"FieldPolicy applies to method (non-field) type ${tp.show}")
      tp
    }
  }

  /** A policy for handling a method or poly. */
  private case class MethodP(trigger: Symbol => Boolean,
                             nnParams: Seq[Int],
                             nnRes: Boolean,
                             nnShallow: Boolean)(implicit ctx: Context) extends TypeMap with NullifyPolicy {
    override def isApplicable(sym: Symbol): Boolean = trigger(sym)

    private def spare(tp: Type): Type = {
      if (nnShallow) nullifyType(tp).stripNull
      else tp
    }

    override def apply(tp: Type): Type = {
      tp match {
        case ptp: PolyType =>
          derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
        case mtp: MethodType =>
          val paramTpes = mtp.paramInfos.zipWithIndex.map {
            case (paramInfo, index) =>
              // TODO(abeln): the sequence lookup can be optimized, because the indices
              // in it appear in increasing order.
              if (nnParams.contains(index)) spare(paramInfo) else nullifyType(paramInfo)
          }
          val resTpe = if (nnRes) spare(mtp.resType) else nullifyType(mtp.resType)
          derivedLambdaType(mtp)(paramTpes, resTpe)
      }
    }
  }

  /** A policy that nullifies only method parameters (but not result types). */
  private def paramsOnlyP(trigger: Symbol => Boolean)(implicit ctx: Context): MethodP = {
    MethodP(trigger, nnParams = Seq.empty, nnRes = true, nnShallow = false)
  }

  private def stdLibP(sym: Symbol, tp: Type)(implicit ctx: Context): NullifyPolicy = {
    val ownerName = sym.owner.showFullName
    if (!nullStats.contains(ownerName)) return FalseP
    val stats = nullStats(ownerName)
    if (sym.is(Flags.Method)) {
      stats.getMethod(sym, tp) match {
        case Some(mstats) =>
          def triggerAndLog(s: Symbol): Boolean = {
//            if (mstats.nnRet) println(s">>> ${sym.name.show} with type ${tp.show}")
            s == sym
          }
          MethodP(triggerAndLog, /*mstats.nnParams*/ Seq.empty, mstats.nnRet, nnShallow = true)
        case None => FalseP
      }
    } else {
      stats.getField(sym, tp) match {
        case Some(fstas) =>
          FieldP(_ == sym)
        case None => FalseP
      }
    }
  }

  /** A wrapper policy that works as `inner` but additionally verifies that the symbol is contained in `owner`. */
  private case class WithinSym(inner: NullifyPolicy, owner: Symbol)(implicit ctx: Context) extends NullifyPolicy {
    override def isApplicable(sym: Symbol): Boolean = sym.owner == owner && inner.isApplicable(sym)

    override def apply(tp: Type): Type = inner(tp)
  }

  /** Nullifies a Java type by adding `| JavaNull` in the relevant places.
   *  We need this because Java types remain implicitly nullable.
   */
  private def nullifyType(tpe: Type)(implicit ctx: Context): Type = {
    val nullMap = new JavaNullMap(alreadyNullable = false)
    nullMap(tpe)
  }

  /** A type map that adds `| JavaNull`.
   *  @param alreadyNullable whether the type being mapped is already nullable (at the outermost level)
   */
  private class JavaNullMap(alreadyNullable: Boolean)(implicit ctx: Context) extends TypeMap {
    /** Should we nullify `tp` at the outermost level? */
    def shouldNullify(tp: Type): Boolean = {
      !alreadyNullable && (tp match {
        case tp: TypeRef => !tp.symbol.isValueClass && !tp.isRef(defn.AnyClass) && !tp.isRef(defn.RefEqClass)
        case _ => true
      })
    }

    /** Should we nullify the arguments to the given generic `tp`?
     *  We only nullify the inside of Scala-defined constructors.
     *  This is because Java classes are _all_ nullified, so both `java.util.List[String]` and
     *  `java.util.List[String|Null]` contain nullable elements.
     */
    def shouldDescend(tp: AppliedType): Boolean = {
      val AppliedType(tycons, _) = tp
      tycons.widenDealias match {
        case tp: TypeRef if !tp.symbol.is(JavaDefined) => true
        case _ => false
      }
    }

    override def apply(tp: Type): Type = {
      tp match {
        case tp: LambdaType => mapOver(tp)
        case tp: TypeAlias => mapOver(tp)
        case tp@AndType(tp1, tp2) =>
          // nullify(A & B) = (nullify(A) & nullify(B)) | Null, but take care not to add
          // duplicate `Null`s at the outermost level inside `A` and `B`.
          val newMap = new JavaNullMap(alreadyNullable = true)
          derivedAndType(tp, newMap(tp1), newMap(tp2)).toJavaNullable
        case tp@OrType(tp1, tp2) if !tp.isJavaNullableUnion =>
          val newMap = new JavaNullMap(alreadyNullable = true)
          derivedOrType(tp, newMap(tp1), newMap(tp2)).toJavaNullable
        case tp: TypeRef if shouldNullify(tp) => tp.toJavaNullable
        case tp: TypeParamRef if shouldNullify(tp) => tp.toJavaNullable
        case appTp@AppliedType(tycons, targs) =>
          val targs2 = if (shouldDescend(appTp)) targs map this else targs
          derivedAppliedType(appTp, tycons, targs2).toJavaNullable
        case _ => tp
      }
    }
  }
}
