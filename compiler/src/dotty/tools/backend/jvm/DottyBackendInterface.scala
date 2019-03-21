package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc
import dotty.tools.dotc.core.Flags.{termFlagSet, termFlagConjunction}
import dotty.tools.dotc.transform.{Erasure, GenericSignatures}
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => _}

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import dotty.tools.dotc.util.WeakHashSet
import dotty.tools.io.AbstractFile
import scala.tools.asm.AnnotationVisitor
import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._
import Phases._

import dotty.tools.dotc.util
import dotty.tools.dotc.util.Spans
import Decorators._
import tpd._

import scala.tools.asm
import StdNames.{nme, str}
import NameKinds.{DefaultGetterName, ExpandedName}
import Names.TermName
import scala.ExplicitNulls._

class DottyBackendInterface(outputDirectory: AbstractFile, val superCallsMap: Map[Symbol, Set[ClassSymbol]])(implicit ctx: Context) extends BackendInterface{
  import Symbols.{toDenot, toClassDenot}
    // Dotty deviation: Need to (re-)import implicit decorators here because otherwise
    // they would be shadowed by the more deeply nested `symHelper` decorator.

  type Symbol          = Nullable[Symbols.Symbol]
  type Type            = Nullable[Types.Type]
  type Tree            = Nullable[tpd.Tree]
  type CompilationUnit = dotc.CompilationUnit
  type Constant        = Nullable[Constants.Constant]
  type Literal         = Nullable[tpd.Literal]
  type Position        = Spans.Span
  type Name            = Nullable[Names.Name]
  type ClassDef        = Nullable[tpd.TypeDef]
  type TypeDef         = Nullable[tpd.TypeDef]
  type Apply           = Nullable[tpd.Apply]
  type TypeApply       = Nullable[tpd.TypeApply]
  type Try             = Nullable[tpd.Try]
  type Assign          = Nullable[tpd.Assign]
  type Ident           = Nullable[tpd.Ident]
  type If              = Nullable[tpd.If]
  type ValDef          = Nullable[tpd.ValDef]
  type Throw           = Nullable[tpd.Apply]
  type Labeled         = Nullable[tpd.Labeled]
  type Return          = Nullable[tpd.Return]
  type WhileDo         = Nullable[tpd.WhileDo]
  type Block           = Nullable[tpd.Block]
  type Typed           = Nullable[tpd.Typed]
  type Match           = Nullable[tpd.Match]
  type This            = Nullable[tpd.This]
  type CaseDef         = Nullable[tpd.CaseDef]
  type Alternative     = Nullable[tpd.Alternative]
  type DefDef          = Nullable[tpd.DefDef]
  type Template        = Nullable[tpd.Template]
  type Select          = Nullable[tpd.Tree] // Actually tpd.Select || tpd.Ident
  type Bind            = Nullable[tpd.Bind]
  type New             = Nullable[tpd.New]
  type Super           = Nullable[tpd.Super]
  type Modifiers       = Null
  type Annotation      = Nullable[Annotations.Annotation]
  type ArrayValue      = Nullable[tpd.JavaSeqLiteral]
  type ApplyDynamic    = Null
  type ModuleDef       = Null
  type LabelDef        = Null
  type Closure         = Nullable[tpd.Closure]

  val NoSymbol: Symbol = Symbols.NoSymbol
  val NoPosition: Position = Spans.NoSpan
  val EmptyTree: Tree = tpd.EmptyTree


  val UnitTag: ConstantTag = Constants.UnitTag
  val IntTag: ConstantTag = Constants.IntTag
  val FloatTag: ConstantTag = Constants.FloatTag
  val NullTag: ConstantTag = Constants.NullTag
  val BooleanTag: ConstantTag = Constants.BooleanTag
  val ByteTag: ConstantTag = Constants.ByteTag
  val ShortTag: ConstantTag = Constants.ShortTag
  val CharTag: ConstantTag = Constants.CharTag
  val DoubleTag: ConstantTag = Constants.DoubleTag
  val LongTag: ConstantTag = Constants.LongTag
  val StringTag: ConstantTag = Constants.StringTag
  val ClazzTag: ConstantTag = Constants.ClazzTag
  val EnumTag: ConstantTag = Constants.EnumTag

  val nme_This: Name = StdNames.nme.This
  val nme_EMPTY_PACKAGE_NAME: Name = StdNames.nme.EMPTY_PACKAGE
  val nme_CONSTRUCTOR: Name = StdNames.nme.CONSTRUCTOR
  val nme_WILDCARD: Name = StdNames.nme.WILDCARD
  val nme_THIS: Name = StdNames.nme.THIS
  val nme_PACKAGE: Name = StdNames.nme.PACKAGE
  val nme_EQEQ_LOCAL_VAR: Name = StdNames.nme.EQEQ_LOCAL_VAR

   // require LambdaMetafactory: scalac uses getClassIfDefined, but we need those always.
  override lazy val LambdaMetaFactory: ClassSymbol = ctx.requiredClass("java.lang.invoke.LambdaMetafactory")
  override lazy val MethodHandle: ClassSymbol      = ctx.requiredClass("java.lang.invoke.MethodHandle")

  val nme_valueOf: Name = StdNames.nme.valueOf
  val nme_apply: TermName = StdNames.nme.apply
  val NothingClass: Symbol = defn.NothingClass
  val NullClass: Symbol = defn.NullClass
  val ObjectClass: Symbol = defn.ObjectClass
  val Object_Type: Type = defn.ObjectType
  val Throwable_Type: Type = defn.ThrowableType
  val Object_isInstanceOf: Symbol = defn.Any_isInstanceOf
  val Object_asInstanceOf: Symbol = defn.Any_asInstanceOf
  val Object_synchronized: Symbol = defn.Object_synchronized
  val Object_equals: Symbol = defn.Any_equals
  val ArrayClass: Symbol = defn.ArrayClass
  val UnitClass: Symbol = defn.UnitClass
  val BooleanClass: Symbol = defn.BooleanClass
  val CharClass: Symbol = defn.CharClass
  val ShortClass: Symbol = defn.ShortClass
  val ClassClass: Symbol = defn.ClassClass
  val ByteClass: Symbol = defn.ByteClass
  val IntClass: Symbol = defn.IntClass
  val LongClass: Symbol = defn.LongClass
  val FloatClass: Symbol = defn.FloatClass
  val DoubleClass: Symbol = defn.DoubleClass
  def isArrayClone(tree: Tree): Boolean = tree match {
    case Select(qual, StdNames.nme.clone_) if qual.tpe.nn.widen.isInstanceOf[JavaArrayType] => true
    case _ => false
  }

  val hashMethodSym: Symbol = NoSymbol // used to dispatch ## on primitives to ScalaRuntime.hash. Should be implemented by a miniphase
  val externalEqualsNumNum: Symbol = defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumNum)
  val externalEqualsNumChar: Symbol = NoSymbol // ctx.requiredMethod(BoxesRunTimeTypeRef, nme.equalsNumChar) // this method is private
  val externalEqualsNumObject: Symbol = defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumObject)
  val externalEquals: Symbol = defn.BoxesRunTimeClass.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol
  val MaxFunctionArity: Int = Definitions.MaxImplementedFunctionArity
  val FunctionClass: Array[Symbol] = defn.FunctionClassPerRun().asInstanceOf[Array[Symbol]]
  val AbstractFunctionClass: Array[Symbol] = defn.AbstractFunctionClassPerRun().asInstanceOf[Array[Symbol]]
  val PartialFunctionClass: Symbol = defn.PartialFunctionClass
  val AbstractPartialFunctionClass: Symbol = defn.AbstractPartialFunctionClass
  val String_valueOf: Symbol = defn.String_valueOf_Object
  lazy val Predef_classOf: Symbol = defn.ScalaPredefModule.requiredMethod(nme.classOf)

  lazy val AnnotationRetentionAttr: ClassSymbol = ctx.requiredClass("java.lang.annotation.Retention")
  lazy val AnnotationRetentionSourceAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("SOURCE")
  lazy val AnnotationRetentionClassAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("CLASS")
  lazy val AnnotationRetentionRuntimeAttr: TermSymbol = ctx.requiredClass("java.lang.annotation.RetentionPolicy").linkedClass.requiredValue("RUNTIME")
  lazy val JavaAnnotationClass: ClassSymbol = ctx.requiredClass("java.lang.annotation.Annotation")

  def boxMethods: Map[Symbol, Symbol] = defn.ScalaValueClasses().map{x => // @darkdimius Are you sure this should be a def?
    (x, Erasure.Boxing.boxMethod(x.asClass))
  }.toMap
  def unboxMethods: Map[Symbol, Symbol] =
    defn.ScalaValueClasses().map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap

  override def isSyntheticArrayConstructor(s: Symbol): Boolean = {
    s eq defn.newArrayMethod
  }

  def isBox(sym: Symbol): Boolean =
    Erasure.Boxing.isBox(sym.nn) && sym.nn.denot.owner != defn.UnitModuleClass
  def isUnbox(sym: Symbol): Boolean =
    Erasure.Boxing.isUnbox(sym.nn) && sym.nn.denot.owner != defn.UnitModuleClass

  val primitives: Primitives = new Primitives {
    val primitives = new DottyPrimitives(ctx)
    def getPrimitive(app: Apply, receiver: Type): Int = primitives.getPrimitive(app.nn, receiver.nn)

    def getPrimitive(sym: Symbol): Int = primitives.getPrimitive(sym.nn)

    def isPrimitive(fun: Tree): Boolean = primitives.isPrimitive(fun.nn)
  }
  implicit val TypeDefTag: ClassTag[TypeDef] = ClassTag[TypeDef](classOf[TypeDef])
  implicit val ApplyTag: ClassTag[Apply] = ClassTag[Apply](classOf[Apply])
  implicit val SelectTag: ClassTag[Select] = ClassTag[Select](classOf[Select])
  implicit val TypeApplyTag: ClassTag[TypeApply] = ClassTag[TypeApply](classOf[TypeApply])
  implicit val ClassDefTag: ClassTag[ClassDef] = ClassTag[TypeDef](classOf[TypeDef])
  implicit val TryTag: ClassTag[Try] = ClassTag[Try](classOf[Try])
  implicit val AssignTag: ClassTag[Assign] = ClassTag[Assign](classOf[Assign])
  implicit val IdentTag: ClassTag[Ident] = ClassTag[Ident](classOf[Ident])
  implicit val IfTag: ClassTag[If] = ClassTag[If](classOf[If])
  implicit val LabelDefTag: ClassTag[LabelDef] = ClassTag[LabelDef](classOf[LabelDef])
  implicit val ValDefTag: ClassTag[ValDef] = ClassTag[ValDef](classOf[ValDef])
  implicit val ThrowTag: ClassTag[Throw] = ClassTag[Throw](classOf[Throw])
  implicit val LabeledTag: ClassTag[Labeled] = ClassTag[Labeled](classOf[Labeled])
  implicit val ReturnTag: ClassTag[Return] = ClassTag[Return](classOf[Return])
  implicit val WhileDoTag: ClassTag[WhileDo] = ClassTag[WhileDo](classOf[WhileDo])
  implicit val LiteralTag: ClassTag[Literal] = ClassTag[Literal](classOf[Literal])
  implicit val BlockTag: ClassTag[Block] = ClassTag[Block](classOf[Block])
  implicit val TypedTag: ClassTag[Typed] = ClassTag[Typed](classOf[Typed])
  implicit val ArrayValueTag: ClassTag[ArrayValue] = ClassTag[ArrayValue](classOf[ArrayValue])
  implicit val MatchTag: ClassTag[Match] = ClassTag[Match](classOf[Match])
  implicit val CaseDefTag: ClassTag[CaseDef] = ClassTag[CaseDef](classOf[CaseDef])
  implicit val ThisTag: ClassTag[This] = ClassTag[This](classOf[This])
  implicit val AlternativeTag: ClassTag[Alternative] = ClassTag[Alternative](classOf[Alternative])
  implicit val DefDefTag: ClassTag[DefDef] = ClassTag[DefDef](classOf[DefDef])
  implicit val ModuleDefTag: ClassTag[ModuleDef] = ClassTag[ModuleDef](classOf[ModuleDef])
  implicit val NameTag: ClassTag[Name] = ClassTag[Name](classOf[Name])
  implicit val TemplateTag: ClassTag[Template] = ClassTag[Template](classOf[Template])
  implicit val BindTag: ClassTag[Bind] = ClassTag[Bind](classOf[Bind])
  implicit val NewTag: ClassTag[New] = ClassTag[New](classOf[New])
  implicit val ApplyDynamicTag: ClassTag[ApplyDynamic] = ClassTag[ApplyDynamic](classOf[ApplyDynamic])
  implicit val SuperTag: ClassTag[Super] = ClassTag[Super](classOf[Super])
  implicit val ConstantClassTag: ClassTag[Constant] = ClassTag[Constant](classOf[Constant])
  implicit val ClosureTag: ClassTag[Closure] = ClassTag[Closure](classOf[Closure])

  def isRuntimeVisible(annot: Annotation): Boolean =
    if (toDenot(annot.atp.typeSymbol.nn).hasAnnotation(AnnotationRetentionAttr))
      retentionPolicyOf(annot) == AnnotationRetentionRuntimeAttr
    else {
      // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
      // annotation is emitted with visibility `RUNTIME`
      // dotty bug: #389
      true
    }

  def shouldEmitAnnotation(annot: Annotation): Boolean = {
    annot.symbol.isJavaDefined &&
      retentionPolicyOf(annot) != AnnotationRetentionSourceAttr &&
      annot.args.isEmpty
  }

  private def retentionPolicyOf(annot: Annotation): Symbol =
    annot.atp.typeSymbol.nn.getAnnotation(AnnotationRetentionAttr).
      flatMap(_.argumentConstant(0).map(_.symbolValue)).getOrElse(AnnotationRetentionClassAttr)

  private def normalizeArgument(arg: Tree): Tree = arg match {
    case Trees.NamedArg(_, arg1) => normalizeArgument(arg1)
    case Trees.Typed(arg1, _) => normalizeArgument(arg1)
    case _ => arg
  }

  private def emitArgument(av:   AnnotationVisitor,
                           name: Nullable[String],
                           arg:  Tree, bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    (normalizeArgument(arg): @unchecked) match {
      case Literal(const @ Constant(_)) =>
        const.tag match {
          case BooleanTag | ByteTag | ShortTag | CharTag | IntTag | LongTag | FloatTag | DoubleTag => av.visit(name, const.value)
          case StringTag =>
            assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
            av.visit(name, const.stringValue) // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
          case ClazzTag => av.visit(name, const.typeValue.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
          case EnumTag =>
            val edesc = innerClasesStore.typeDescriptor(const.nn.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
            val evalue = const.symbolValue.name.mangledString // value the actual enumeration value.
            av.visitEnum(name, edesc, evalue)
        }
      case t: TypeApply if (t.nn.fun.symbol == Predef_classOf) =>
        av.visit(name, t.nn.args.head.tpe.classSymbol.denot.info.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
      case t: tpd.Select =>
        if (t.symbol.denot.owner.is(Flags.JavaEnum)) {
          val edesc = innerClasesStore.typeDescriptor(t.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
          val evalue = t.symbol.name.mangledString // value the actual enumeration value.
          av.visitEnum(name, edesc, evalue)
        } else {
            // println(i"not an enum: ${t.symbol} / ${t.symbol.denot.owner} / ${t.symbol.denot.owner.isTerm} / ${t.symbol.denot.owner.flags}")
            assert(toDenot(t.symbol).name.is(DefaultGetterName),
              s"${toDenot(t.symbol).name.debugString}") // this should be default getter. do not emit.
        }
      case t: SeqLiteral =>
        val arrAnnotV: AnnotationVisitor = av.visitArray(name).nn
        for (arg <- t.elems) { emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore) }
        arrAnnotV.visitEnd()

      case Apply(fun, args) if fun.symbol == defn.ArrayClass.primaryConstructor ||
        toDenot(fun.symbol.nn).owner == defn.ArrayClass.linkedClass && fun.symbol.name == nme_apply =>
        val arrAnnotV: AnnotationVisitor = av.visitArray(name).nn

        var actualArgs = if (fun.tpe.nn.isImplicitMethod) {
          // generic array method, need to get implicit argument out of the way
          fun.asInstanceOf[Apply].nn.args
        } else args

        val flatArgs = actualArgs.flatMap { arg =>
          normalizeArgument(arg) match {
            case t: tpd.SeqLiteral => t.elems
            case e => List(e)
          }
        }
        for(arg <- flatArgs) {
          emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore)
        }
        arrAnnotV.visitEnd()
/*
      case sb @ ScalaSigBytes(bytes) =>
        // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
        // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
        if (sb.fitsInOneString) {
          av.visit(name, BCodeAsmCommon.strEncode(sb))
        } else {
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- BCodeAsmCommon.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
          arrAnnotV.visitEnd()
        }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.
*/
      case t @ Apply(constr, args) if t.tpe.nn.derivesFrom(JavaAnnotationClass) =>
        val typ = t.tpe.nn.classSymbol.denot.info
        val assocs = assocsFromApply(t)
        val desc = innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the nested annotation class
        val nestedVisitor = av.visitAnnotation(name, desc).nn
        emitAssocs(nestedVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  override def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = cw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot)).nn
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, Nullable[Object])], bcodeStore: BCodeHelpers)
                        (innerClasesStore: bcodeStore.BCInnerClassGen) = {
    for ((name, value) <- assocs)
      emitArgument(av, name.mangledString, value.asInstanceOf[Tree], bcodeStore)(innerClasesStore)
    av.visitEnd()
  }

  override def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = mw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot)).nn
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  override def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)
                              (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val typ = annot.atp
      val assocs = annot.assocs
      val av = fw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot)).nn
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  override def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]], bcodeStore: BCodeHelpers)
                                   (innerClasesStore: bcodeStore.BCInnerClassGen): Unit = {
    val annotationss = pannotss map (_ filter shouldEmitAnnotation)
    if (annotationss forall (_.isEmpty)) return
    for ((annots, idx) <- annotationss.zipWithIndex;
         annot <- annots) {
      val typ = annot.atp
      val assocs = annot.assocs
      val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot)).nn
      emitAssocs(pannVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def getAnnotPickle(jclassName: String, sym: Symbol): Option[Annotation] = None


  def getRequiredClass(fullname: String): Symbol = ctx.requiredClass(fullname)

  def getClassIfDefined(fullname: String): Symbol = NoSymbol // used only for android. todo: implement

  private def erasureString(clazz: Class[_]): String = {
    if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType.nn) + "]"
    else clazz.getName
  }

  def requiredClass[T](implicit evidence: ClassTag[T]): Symbol =
    ctx.requiredClass(erasureString(evidence.runtimeClass))

  def requiredModule[T](implicit evidence: ClassTag[T]): Symbol = {
    val moduleName = erasureString(evidence.runtimeClass)
    val className = if (moduleName.endsWith("$")) moduleName.dropRight(1)  else moduleName
    ctx.requiredModule(className)
  }

  def debuglog(msg: => String): Unit = ctx.debuglog(msg)
  def informProgress(msg: String): Unit = ctx.informProgress(msg)
  def log(msg: => String): Unit = ctx.log(msg)
  def error(pos: Position, msg: String): Unit = ctx.error(msg, sourcePos(pos))
  def warning(pos: Position, msg: String): Unit = ctx.warning(msg, sourcePos(pos))
  def abort(msg: String): Nothing = {
    ctx.error(msg)
    throw new RuntimeException(msg)
  }
  def sourcePos(pos: Position)(implicit ctx: Context): util.SourcePosition =
    ctx.source.atSpan(pos)

  def emitAsmp: Option[String] = None

  def hasLabelDefs: Boolean = false

  def dumpClasses: Option[String] =
    if (ctx.settings.Ydumpclasses.isDefault) None
    else Some(ctx.settings.Ydumpclasses.value)

  def mainClass: Option[String] =
    if (ctx.settings.XmainClass.isDefault) None
    else Some(ctx.settings.XmainClass.value)
  def setMainClass(name: String): Unit = ctx.settings.XmainClass.update(name)


  def noForwarders: Boolean = ctx.settings.XnoForwarders.value
  def debuglevel: Int = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames
  def settings_debug: Boolean = ctx.settings.Ydebug.value
  def targetPlatform: String = ctx.settings.target.value

  val perRunCaches: Caches = new Caches {
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = new mutable.AnyRefMap[K, V]()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = new mutable.WeakHashMap[K, V]()
    def recordCache[T <: Clearable](cache: T): T = cache
    def newWeakSet[K >: Null <: AnyRef](): WeakHashSet[K] = new WeakHashSet[K]()
    def newMap[K, V](): mutable.HashMap[K, V] = new mutable.HashMap[K, V]()
    def newSet[K](): mutable.Set[K] = new mutable.HashSet[K]
  }

  val MODULE_INSTANCE_FIELD: String = str.MODULE_INSTANCE_FIELD

  def dropModule(str: String): String =
    if (!str.isEmpty && str.last == '$') str.take(str.length - 1) else str

  def newTermName(prefix: String): Name = prefix.toTermName

  val Flag_SYNTHETIC: Flags = Flags.Synthetic.bits
  val Flag_METHOD: Flags = Flags.Method.bits
  val ExcludedForwarderFlags: Flags = {
      Flags.Specialized | Flags.Lifted | Flags.Protected | Flags.JavaStatic |
      Flags.Bridge | Flags.Private | Flags.Macro
  }.bits

  def isQualifierSafeToElide(qual: Tree): Boolean = tpd.isIdempotentExpr(qual.nn)

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def desugarIdent(i: Ident): Option[tpd.Select] = {
    var found = desugared.get(i.tpe)
    if (found == null) {
      tpd.desugarIdent(i.nn) match {
        case sel: tpd.Select =>
          desugared.put(i.tpe, sel)
          found = sel
        case _ =>
      }
    }
    if (found == null) None else Some(found.nn)
  }

  def getLabelDefOwners(tree: Tree): Map[Tree, List[LabelDef]] = Map.empty

  // todo: remove
  def isMaybeBoxed(sym: Symbol): Boolean = {
    val sym1 = sym.nn
    (sym1 == ObjectClass) ||
      (sym1 == JavaSerializableClass) ||
      (sym1 == defn.ComparableClass) ||
      (sym1 derivesFrom BoxedNumberClass.nn) ||
      (sym1 derivesFrom BoxedCharacterClass.nn) ||
      (sym1 derivesFrom BoxedBooleanClass.nn)
  }

  def getSingleOutput: Option[AbstractFile] = None // todo: implement

  // @M don't generate java generics sigs for (members of) implementation
  // classes, as they are monomorphic (TODO: ok?)
  private final def needsGenericSignature(sym: Symbol): Boolean = !(
    // pp: this condition used to include sym.hasexpandedname, but this leads
    // to the total loss of generic information if a private member is
    // accessed from a closure: both the field and the accessor were generated
    // without it.  This is particularly bad because the availability of
    // generic information could disappear as a consequence of a seemingly
    // unrelated change.
       ctx.base.settings.YnoGenericSig.value || {
         val sym1 = sym.nn
         sym1.is(Flags.Artifact) || sym1.is(Flags.LiftedMethod) || sym1.is(Flags.Bridge)
       }
  )

  private def verifySignature(sym: Symbol, sig: String)(implicit ctx: Context): Unit = {
    import scala.tools.asm.util.CheckClassAdapter
    def wrap(body: => Unit): Boolean =
      try { body; true }
      catch { case ex: Throwable => println(ex.getMessage); false }

    val valid = wrap {
      val sym1 = sym.nn
      if (sym1.is(Flags.Method)) {
        CheckClassAdapter.checkMethodSignature(sig)
      }
      else if (sym1.isTerm) {
        CheckClassAdapter.checkFieldSignature(sig)
      }
      else {
        CheckClassAdapter.checkClassSignature(sig)
      }
    }

    if (!valid) {
      ctx.error(
        i"""|compiler bug: created invalid generic signature for $sym in ${sym.nn.denot.owner.showFullName}
            |signature: $sig
            |if this is reproducible, please report bug at https://github.com/lampepfl/dotty/issues
        """.trim, sym.nn.sourcePos)
    }
  }

  /**
   * Generates the generic signature for `sym` before erasure.
   *
   * @param sym   The symbol for which to generate a signature.
   * @param owner The owner of `sym`.
   * @return The generic signature of `sym` before erasure, as specified in the Java Virtual
   *         Machine Specification, §4.3.4, or `null` if `sym` doesn't need a generic signature.
   * @see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3.4
   */
  def getGenericSignature(sym: Symbol, owner: Symbol): Nullable[String] = {
    ctx.atPhase(ctx.erasurePhase) { implicit ctx =>
      val memberTpe =
        if (sym.nn.is(Flags.Method)) sym.nn.denot.info
        else owner.nn.denot.thisType.memberInfo(sym)
      getGenericSignature(sym, owner, memberTpe).orNull
    }
  }

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): Nullable[String] = {
    // scala/bug#3452 Static forwarder generation uses the same erased signature as the method if forwards to.
    // By rights, it should use the signature as-seen-from the module class, and add suitable
    // primitive and value-class boxing/unboxing.
    // But for now, just like we did in mixin, we just avoid writing a wrong generic signature
    // (one that doesn't erase to the actual signature). See run/t3452b for a test case.

    val memberTpe = ctx.atPhase(ctx.erasurePhase) { implicit ctx => moduleClass.nn.denot.thisType.memberInfo(sym) }
    val erasedMemberType = TypeErasure.erasure(memberTpe.nn)
    if (erasedMemberType =:= sym.nn.denot.info)
      getGenericSignature(sym, moduleClass, memberTpe).orNull
    else null
  }

  private def getGenericSignature(sym: Symbol, owner: Symbol, memberTpe: Type)(implicit ctx: Context): Option[String] =
    if (needsGenericSignature(sym)) {
      val erasedTypeSym = sym.nn.denot.info.typeSymbol
      if (erasedTypeSym.isPrimitiveValueClass) {
        None
      } else {
        val jsOpt = GenericSignatures.javaSig(sym.nn, memberTpe.nn)
        if (ctx.settings.XverifySignatures.value) {
          jsOpt.foreach(verifySignature(sym, _))
        }

        jsOpt
      }
    } else {
      None
    }



  def sourceFileFor(cu: CompilationUnit): String = cu.source.file.name



  implicit def positionHelper(a: Position): PositionHelper = new PositionHelper {
    def isDefined: Boolean = a.exists
    def line: Int = sourcePos(a).line + 1
    def finalPosition: Position = a
  }

  implicit def constantHelper(a1: Constant): ConstantHelper = new ConstantHelper {
    val a: Constants.Constant = a1.nn
    def booleanValue: Boolean = a.booleanValue
    def longValue: Long = a.longValue
    def byteValue: Byte = a.byteValue
    def stringValue: String = a.stringValue
    def symbolValue: Symbol = a.symbolValue
    def floatValue: Float = a.floatValue
    def value: Any = a.value
    def tag: ConstantTag = a.tag
    def typeValue: Type = a.typeValue
    def shortValue: Short = a.shortValue
    def intValue: Int = a.intValue
    def doubleValue: Double = a.doubleValue
    def charValue: Char = a.charValue
  }


  implicit def treeHelper(a1: Tree): TreeHelper = {
    val a = a1.nn
    new TreeHelper {
      def symbol: Symbol = a.symbol

      def pos: Position = a.span

      def isEmpty: Boolean = a.isEmpty

      def tpe: Type = a.tpe

      def exists(pred: (Tree) => Boolean): Boolean = a.find(pred).isDefined
    }
  }


  implicit def annotHelper(a1: Annotation): AnnotationHelper = {
    val a = a1.nn
    new AnnotationHelper {
      def atp: Type = a.tree.tpe

      def assocs: List[(Name, Tree)] = assocsFromApply(a.tree)

      def symbol: Symbol = a.tree.symbol

      def args: List[Tree] = List.empty // those arguments to scala-defined annotations. they are never emitted
    }
  }

  def assocsFromApply(tree: Tree): List[(Name, Tree)] = {
    (tree: @unchecked) match {
      case Block(_, expr) => assocsFromApply(expr)
      case Apply(fun, args) =>
        fun.tpe.nn.widen match {
          case MethodType(names) =>
            (names zip args).filter {
              case (_, t: tpd.Ident) if (t.tpe.normalizedPrefix eq NoPrefix) => false
              case _ => true
            }
        }
    }
  }

  implicit def nameHelper(n: Name): NameHelper = new NameHelper {
    def isTypeName: Boolean = n.isTypeName
    def isTermName: Boolean = n.isTermName
    def startsWith(s: String): Boolean = n.startsWith(s)
    def mangledString: String = n.mangledString
  }

  implicit def symHelper(sym1: Symbol): SymbolHelper = new SymbolHelper {
    private val sym = sym1.nn

    // names
    def fullName(sep: Char): String = sym.showFullName
    def fullName: String = sym.showFullName
    def simpleName: Name = sym.name
    def javaSimpleName: String = toDenot(sym).name.mangledString // addModuleSuffix(simpleName.dropLocal)
    def javaBinaryName: String = javaClassName.replace('.', '/') // TODO: can we make this a string? addModuleSuffix(fullNameInternal('/'))
    def javaClassName: String = toDenot(sym).fullName.mangledString // addModuleSuffix(fullNameInternal('.')).toString
    def name: Name = sym.name
    def rawname: String = {
      val original = toDenot(sym).initial
      sym.name(ctx.withPhase(original.validFor.phaseId)).mangledString
    }

    // types
    def info: Type = toDenot(sym).info
    def tpe: Type = toDenot(sym).info // todo whats the differentce between tpe and info?
    def thisType: Type = toDenot(sym).thisType

    // tests
    def isClass: Boolean = {
      sym.isPackageObject || (sym.isClass)
    }
    def isType: Boolean = sym.isType
    def isAnonymousClass: Boolean = toDenot(sym).isAnonymousClass
    def isConstructor: Boolean = toDenot(sym).isConstructor
    def isExpanded: Boolean = sym.name.is(ExpandedName)
    def isAnonymousFunction: Boolean = toDenot(sym).isAnonymousFunction
    def isMethod: Boolean = sym is Flags.Method
    def isPublic: Boolean =  sym.flags.is(Flags.EmptyFlags, Flags.Private | Flags.Protected)
    def isSynthetic: Boolean = sym is Flags.Synthetic
    def isPackageClass: Boolean = sym is Flags.PackageClass
    def isModuleClass: Boolean = sym is Flags.ModuleClass
    def isModule: Boolean = sym is Flags.Module
    def isStrictFP: Boolean = false // todo: implement
    def isLabel: Boolean = sym is Flags.Label
    def hasPackageFlag: Boolean = sym is Flags.Package
    def isImplClass: Boolean = sym is Flags.ImplClass
    def isInterface: Boolean = (sym is Flags.PureInterface) || (sym is Flags.Trait)
    def isGetter: Boolean = toDenot(sym).isGetter
    def isSetter: Boolean = toDenot(sym).isSetter
    def isGetClass: Boolean = sym eq defn.Any_getClass
    def isJavaDefined: Boolean = sym is Flags.JavaDefined
    def isJavaDefaultMethod: Boolean = !((sym is Flags.Deferred)  || toDenot(sym).isClassConstructor)
    def isDeferred: Boolean = sym is Flags.Deferred
    def isPrivate: Boolean = sym is Flags.Private
    def getsJavaFinalFlag: Boolean =
      isFinal &&  !toDenot(sym).isClassConstructor && !(sym is Flags.Mutable) &&  !(sym.enclosingClass is Flags.Trait)

    def getsJavaPrivateFlag: Boolean =
      isPrivate //|| (sym.isPrimaryConstructor && sym.owner.isTopLevelModuleClass)

    def isFinal: Boolean = sym is Flags.Final
    def isStaticMember: Boolean = (sym ne NoSymbol) &&
      ((sym is Flags.JavaStatic) || (owner is Flags.ImplClass) || toDenot(sym).hasAnnotation(ctx.definitions.ScalaStaticAnnot))
      // guard against no sumbol cause this code is executed to select which call type(static\dynamic) to use to call array.clone

    def isBottomClass: Boolean = (sym ne defn.NullClass) && (sym ne defn.NothingClass)
    def isBridge: Boolean = sym is Flags.Bridge
    def isArtifact: Boolean = sym is Flags.Artifact
    def hasEnumFlag: Boolean = sym is Flags.JavaEnum
    def hasAccessBoundary: Boolean = sym.accessBoundary(defn.RootClass) ne defn.RootClass
    def isVarargsMethod: Boolean = sym is Flags.JavaVarargs
    def isDeprecated: Boolean = false
    def isMutable: Boolean = sym is Flags.Mutable
    def hasAbstractFlag: Boolean =
      (sym is Flags.Abstract) || (sym is Flags.JavaInterface) || (sym is Flags.Trait)
    def hasModuleFlag: Boolean = sym is Flags.Module
    def isSynchronized: Boolean = sym is Flags.Synchronized
    def isNonBottomSubClass(other: Symbol): Boolean = sym.derivesFrom(other.nn)
    def hasAnnotation(ann: Symbol): Boolean = toDenot(sym).hasAnnotation(ann.nn)
    def shouldEmitForwarders: Boolean =
      (sym is Flags.Module) && !(sym is Flags.ImplClass) && sym.isStatic
    def isJavaEntryPoint: Boolean = CollectEntryPoints.isJavaEntryPoint(sym)

    def isClassConstructor: Boolean = toDenot(sym).isClassConstructor
    def isSerializable: Boolean = toDenot(sym).isSerializable

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean =
      (sym is Flags.Module) && {
        // scalac uses atPickling here
        // this would not work if modules are created after pickling
        // for example by specialization
        val original = toDenot(sym).initial
        val validity = original.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        toDenot(sym)(shiftedContext).isStatic(shiftedContext)
      }

    def isStaticConstructor: Boolean = (isStaticMember && isClassConstructor) || (sym.name eq nme.STATIC_CONSTRUCTOR)


    // navigation
    def owner: Symbols.Symbol = toDenot(sym).owner
    def rawowner: Symbol = {
      originalOwner
    }
    def originalOwner: Symbol =
      // used to populate the EnclosingMethod attribute.
      // it is very tricky in presence of classes(and annonymous classes) defined inside supper calls.
      if (sym.exists) {
        val original = toDenot(sym).initial
        val validity = original.validFor
        val shiftedContext = ctx.withPhase(validity.phaseId)
        val r = toDenot(sym)(shiftedContext).maybeOwner.lexicallyEnclosingClass(shiftedContext)
        r
      } else NoSymbol
    def parentSymbols: List[Symbol] = toDenot(sym).info.parents.map(_.typeSymbol)
    def superClass: Symbol =  {
      val t = toDenot(sym).asClass.superClass
      if (t.exists) t
      else if (sym is Flags.ModuleClass) {
        // workaround #371

        println(s"Warning: mocking up superclass for $sym")
        ObjectClass
      }
      else t
    }
    def enclClass: Symbol = toDenot(sym).enclosingClass
    def linkedClassOfClass: Symbol = linkedClass
    def linkedClass: Symbol = toDenot(sym)(ctx).linkedClass(ctx) //exitingPickler(sym.linkedClassOfClass)
    def companionClass: Symbol = toDenot(sym).companionClass
    def companionModule: Symbol = toDenot(sym).companionModule
    def companionSymbol: Symbol = if (sym is Flags.Module) companionClass else companionModule
    def moduleClass: Symbol = toDenot(sym).moduleClass
    def enclosingClassSym: Symbol = {
      if (this.isClass) {
        val ct = ctx.withPhase(ctx.flattenPhase.prev)
        toDenot(sym)(ct).owner.enclosingClass(ct)
      }
      else sym.enclosingClass(ctx.withPhase(ctx.flattenPhase.prev))
    } //todo is handled specially for JavaDefined symbols in scalac

    // members
    def primaryConstructor: Symbol = toDenot(sym).primaryConstructor

    /** For currently compiled classes: All locally defined classes including local classes.
     *  The empty list for classes that are not currently compiled.
     */
    def nestedClasses: List[Symbol] = definedClasses(ctx.flattenPhase)

    /** For currently compiled classes: All classes that are declared as members of this class
     *  (but not inherited ones). The empty list for classes that are not currently compiled.
     */
    def memberClasses: List[Symbol] = definedClasses(ctx.lambdaLiftPhase)

    private def definedClasses(phase: Phase) =
      if (sym.isDefinedInCurrentRun)
        ctx.atPhase(phase) { implicit ctx =>
          toDenot(sym).info.decls.filter(_.isClass)
        }
      else Nil

    def annotations: List[Annotation] = toDenot(sym).annotations
    def companionModuleMembers: List[Symbol] =  {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the example) that were lifted by lambdalift.
      if (linkedClass.isTopLevelModuleClass) /*exitingPickler*/ linkedClass.memberClasses
      else Nil
    }
    def fieldSymbols: List[Symbol] = {
      toDenot(sym).info.decls.filter(p => p.isTerm && !p.is(Flags.Method))
    }
    def methodSymbols: List[Symbol] =
      for (f <- toDenot(sym).info.decls.toList if f.isMethod && f.isTerm && !f.isModule) yield f
    def serialVUID: Option[Long] = None


    def freshLocal(cunit: CompilationUnit, name: String, tpe: Type, pos: Position, flags: Flags): Symbol = {
      ctx.newSymbol(sym, name.toTermName, termFlagSet(flags), tpe.nn, Symbols.NoSymbol, pos)
    }

    def getter(clz: Symbol): Symbol = decorateSymbol(sym).getter
    def setter(clz: Symbol): Symbol = decorateSymbol(sym).setter

    def moduleSuffix: String = "" // todo: validate that names already have $ suffix
    def outputDirectory: AbstractFile = DottyBackendInterface.this.outputDirectory
    def pos: Position = sym.span

    def throwsAnnotations: List[Symbol] = Nil

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     * Redundant interfaces are removed unless there is a super call to them.
     */
    def superInterfaces: List[Symbol] = {
      val directlyInheritedTraits = decorateSymbol(sym).directlyInheritedTraits
      val directlyInheritedTraitsSet = directlyInheritedTraits.toSet
      val allBaseClasses = directlyInheritedTraits.iterator.flatMap(_.asClass.baseClasses.drop(1)).toSet
      val superCalls = superCallsMap.getOrElse(sym, Set.empty)
      val additional = (superCalls -- directlyInheritedTraitsSet).filter(_.is(Flags.Trait))
//      if (additional.nonEmpty)
//        println(s"$fullName: adding supertraits $additional")
      directlyInheritedTraits.filter(t => !allBaseClasses(t) || superCalls(t)) ++ additional
    }

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = sym.isModuleClass &&
      ctx.atPhase(ctx.flattenPhase) { implicit ctx =>
        toDenot(sym).owner.is(Flags.PackageClass)
      }

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean = sym.isStatic


    def addRemoteRemoteExceptionAnnotation: Unit = ()

    def samMethod(): Symbol =
      toDenot(sym).info.abstractTermMembers.headOption.getOrElse(toDenot(sym).info.member(nme.apply)).symbol

    def isFunctionClass: Boolean =
      defn.isFunctionClass(sym)
  }


  implicit def typeHelper(tp1: Type): TypeHelper = new TypeHelper {
    private val tp = tp1.nn

    def member(string: Name): Symbol = tp.member(string.nn.toTermName).symbol

    def isFinalType: Boolean = tp.typeSymbol is Flags.Final //in scalac checks for type parameters. Why? Aren't they gone by backend?

    def underlying: Type = tp match {
      case t: TypeProxy => t.underlying
      case _ => tp
    }

    def paramTypes: List[Type] = tp.firstParamTypes

    def <:<(other: Type): Boolean = tp <:< other

    def memberInfo(s: Symbol): Type = tp.memberInfo(s.nn)

    def decls: List[Symbol] = tp.decls.toList

    def members: List[Symbol] = tp.allMembers.map(_.symbol).toList

    def typeSymbol: Symbol = tp.widenDealias.typeSymbol

    def =:=(other: Type): Boolean = tp =:= other

    def membersBasedOnFlags(excludedFlags: Flags, requiredFlags: Flags): List[Symbol] =
      tp.membersBasedOnFlags(termFlagConjunction(requiredFlags), termFlagSet(excludedFlags)).map(_.symbol).toList

    def resultType: Type = tp.resultType

    def toTypeKind(ct: BCodeHelpers)(storage: ct.BCInnerClassGen): ct.bTypes.BType = {
      import ct.bTypes._
      val defn = ctx.definitions
      import coreBTypes._
      import Types._
      /**
       * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
       * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
       */
      def primitiveOrClassToBType(sym: Symbol): BType = {
        assert(sym.isClass, sym)
        assert(sym != ArrayClass || isCompilingArray, sym)
        primitiveTypeMap.getOrElse(sym.asInstanceOf[ct.bTypes.coreBTypes.bTypes.int.Symbol],
          storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ct.int.Symbol])).asInstanceOf[BType]
      }

      /**
       * When compiling Array.scala, the type parameter T is not erased and shows up in method
       * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
       */
      def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
        assert(sym.isType && isCompilingArray, sym)
        ObjectReference.asInstanceOf[ct.bTypes.ClassBType]
      }

      tp.widenDealias match {
        case JavaArrayType(el) =>ArrayBType(el.toTypeKind(ct)(storage)) // Array type such as Array[Int] (kept by erasure)
        case t: TypeRef =>
          t.info match {

            case _ =>
              if (!t.symbol.isClass) nonClassTypeRefToBType(t.symbol)  // See comment on nonClassTypeRefToBType
              else primitiveOrClassToBType(t.symbol) // Common reference to a type such as scala.Int or java.lang.String
          }
        case Types.ClassInfo(_, sym, _, _, _)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

        /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
         * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
         * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
         */
        case a @ AnnotatedType(t, _) =>
          debuglog(s"typeKind of annotated type $a")
          t.toTypeKind(ct)(storage)

        /* ExistentialType should (probably) be eliminated by erasure. We know they get here for
         * classOf constants:
         *   class C[T]
         *   class T { final val k = classOf[C[_]] }
         */
       /* case e @ ExistentialType(_, t) =>
          debuglog(s"typeKind of existential type $e")
          t.toTypeKind(ctx)(storage)*/

        /* The cases below should probably never occur. They are kept for now to avoid introducing
         * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
         * test suite don't produce any warning.
         */

        case tp =>
          ctx.warning(
            s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
              "If possible, please file a bug on https://github.com/lampepfl/dotty/issues")

          tp match {
            case tp: ThisType if tp.cls == ArrayClass => ObjectReference.asInstanceOf[ct.bTypes.ClassBType] // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
            case tp: ThisType                         => storage.getClassBTypeAndRegisterInnerClass(tp.cls.asInstanceOf[ct.int.Symbol])
           // case t: SingletonType                   => primitiveOrClassToBType(t.classSymbol)
            case t: SingletonType                     => t.underlying.toTypeKind(ct)(storage)
            case t: RefinedType                       =>  t.parent.toTypeKind(ct)(storage) //parents.map(_.toTypeKind(ct)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
          }
      }
    }

    def summaryString: String = tp.showSummary

    def params: List[Symbol] =
      Nil // backend uses this to emit annotations on parameter lists of forwarders
          // to static methods of companion class
          // in Dotty this link does not exists: there is no way to get from method type
          // to inner symbols of DefDef
          // todo: somehow handle.

    def parents: List[Type] = tp.parents
  }

  object Assign extends AssignDeconstructor {
    def _1: Tree = field.nn.lhs
    def _2: Tree = field.nn.rhs
  }

  object Select extends SelectDeconstructor {

    var desugared: Nullable[tpd.Select] = _

    override def isEmpty: Boolean =
      desugared eq null

    def _1: Tree =  desugared.nn.qualifier

    def _2: Name = desugared.nn.name

    override def unapply(s: Select): this.type = {
      s match {
        case t: tpd.Select => desugared = t
        case t: Ident  =>
          desugarIdent(t) match {
            case Some(t) => desugared = t
            case None => desugared = null
          }
        case _ => desugared = null
      }

      this
    }
  }

  object Apply extends ApplyDeconstructor {
    def _1: Tree = field.nn.fun
    def _2: List[Tree] = field.nn.args
  }

  object If extends IfDeconstructor {
    def _1: Tree = field.nn.cond
    def _2: Tree = field.nn.thenp
    def _3: Tree = field.nn.elsep
  }

  object ValDef extends ValDefDeconstructor {
    def _1: Nullable[Modifiers] = null
    def _2: Name = field.nn.name
    def _3: Tree = field.nn.tpt
    def _4: Tree = field.nn.rhs
  }

  object ApplyDynamic extends ApplyDynamicDeconstructor {
    def _1: Tree = ???
    def _2: List[Tree] = ???
  }

  // todo: this product1s should also eventually become name-based pattn matching
  object Literal extends LiteralDeconstructor {
    def get: Constant = field.nn.const
  }

  object Throw extends ThrowDeconstructor {
    def get: Tree = field.nn.args.head

    override def unapply(s: Throw): Throw.type = {
      if (s.nn.fun.symbol eq defn.throwMethod) {
        field = s
      } else {
        field = null
      }
      this
    }
  }

  object New extends NewDeconstructor {
    def get: Type = field.nn.tpt.tpe
  }

  object This extends ThisDeconstructor {
    def get: Name = field.nn.qual.name
    def apply(s: Symbol): This = tpd.This(s.nn.asClass)
  }

  object Labeled extends LabeledDeconstructor {
    def _1: Bind = field.nn.bind
    def _2: Tree = field.nn.expr
  }

  object Return extends ReturnDeconstructor {
    def _1: Tree = field.nn.expr
    def _2: Symbol = if (field.nn.from.symbol.isLabel) field.nn.from.symbol else NoSymbol
  }

  object WhileDo extends WhileDoDeconstructor {
    def _1: Tree = field.nn.cond
    def _2: Tree = field.nn.body
  }

  object Ident extends IdentDeconstructor {
    def get: Name = field.nn.name
  }

  object Alternative extends AlternativeDeconstructor {
    def get: List[Tree] = field.nn.trees
  }

  object Constant extends ConstantDeconstructor {
    def get: Any = field.nn.value
  }
  object ThrownException extends ThrownException {
    def unapply(a: Annotation): Option[Symbol] = None // todo
  }

  object Try extends TryDeconstructor {
    def _1: Tree = field.nn.expr
    def _2: List[Tree] = field.nn.cases
    def _3: Tree = field.nn.finalizer
  }

  object LabelDef extends LabelDeconstructor {
    def _1: Name = ???
    def _2: List[Symbol] = ???
    def _3: Tree = ???
  }

  object Typed extends TypedDeconstrutor {
    def _1: Tree = field.nn.expr
    def _2: Tree = field.nn.tpt
  }
  object Super extends SuperDeconstructor {
    def _1: Tree = field.nn.qual
    def _2: Name = field.nn.mix.name
  }
  object ArrayValue extends ArrayValueDeconstructor {
    def _1: Type = field.nn.tpe match {
      case JavaArrayType(elem) => elem
      case _ =>
        error(field.nn.span, s"JavaSeqArray with type ${field.nn.tpe} reached backend: $field")
        UnspecifiedErrorType
    }
    def _2: List[Tree] = field.nn.elems
  }
  object Match extends MatchDeconstructor {
    def _1: Tree = field.nn.selector
    def _2: List[Tree] = field.nn.cases
  }
  object Block extends BlockDeconstructor {
    def _1: List[Tree] = field.nn.stats
    def _2: Tree = field.nn.expr
  }
  object TypeApply extends TypeApplyDeconstructor {
    def _1: Tree = field.nn.fun
    def _2: List[Tree] = field.nn.args
  }
  object CaseDef extends CaseDeconstructor {
    def _1: Tree = field.nn.pat
    def _2: Tree = field.nn.guard
    def _3: Tree = field.nn.body
  }

  object DefDef extends DefDefDeconstructor {
    def _1: Nullable[Modifiers] = null
    def _2: Name = field.nn.name
    def _3: List[TypeDef] = field.nn.tparams
    def _4: List[List[ValDef]] = field.nn.vparamss
    def _5: Tree = field.nn.tpt
    def _6: Tree = field.nn.rhs
  }

  object ModuleDef extends ModuleDefDeconstructor {
    def _1: Modifiers = ???
    def _2: Name = ???
    def _3: Tree = ???
  }

  object Template extends TemplateDeconstructor {
    def _1: List[Tree] = field.nn.parents
    def _2: ValDef = field.nn.self
    def _3: List[Tree] =
      if (field.nn.constr.rhs.isEmpty) field.nn.body
      else field.nn.constr :: field.nn.body
  }

  object Bind extends BindDeconstructor {
    def _1: Name = field.nn.name
    def _2: Tree = field.nn.body
  }

  object ClassDef extends ClassDefDeconstructor {
    def _1: Nullable[Modifiers] = null
    def _2: Name = field.nn.name
    def _4: Template = field.nn.rhs.asInstanceOf[Template]
    def _3: List[TypeDef] = Nil
  }

  object Closure extends ClosureDeconstructor {
    def _1: List[Tree] = field.nn.env
    def _2: Tree = field.nn.meth
    def _3: Symbol = {
      val t = field.nn.tpt.tpe.typeSymbol
      if (t.exists) t
      else {
        val arity = field.nn.meth.tpe.widenDealias.paramTypes.size - _1.size
        val returnsUnit = field.nn.meth.tpe.widenDealias.resultType.classSymbol == UnitClass
        if (returnsUnit)
          ctx.requiredClass(("dotty.runtime.function.JProcedure" + arity))
        else ctx.requiredClass(("dotty.runtime.function.JFunction" + arity))
      }
    }
  }

  def currentUnit: CompilationUnit = ctx.compilationUnit
}
