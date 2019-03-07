package dotty.tools.dotc
package transform

import scala.annotation.tailrec
import core._
import MegaPhase._
import collection.mutable
import Symbols._, Contexts._, Types._, StdNames._, NameOps._
import ast.Trees._
import util.Spans._
import typer.Applications.{isProductMatch, isGetMatch, productSelectors}
import SymUtils._
import Flags._, Constants._
import Decorators._
import NameKinds.{PatMatStdBinderName, PatMatAltsName, PatMatResultName}
import config.Printers.patmatch
import reporting.diagnostic.messages._
import dotty.tools.dotc.ast._

import scala.ExplicitNulls._

/** The pattern matching transform.
 *  After this phase, the only Match nodes remaining in the code are simple switches
 *  where every pattern is an integer constant
 */
class PatternMatcher extends MiniPhase {
  import ast.tpd._
  import PatternMatcher._

  override def phaseName: String = PatternMatcher.name
  override def runsAfter: Set[String] = Set(ElimRepeated.name)

  override def transformMatch(tree: Match)(implicit ctx: Context): Tree =
    if (tree.isInstanceOf[InlineMatch]) tree
    else {
      val translated = new Translator(tree.tpe, this).translateMatch(tree)

      // check exhaustivity and unreachability
      val engine = new patmat.SpaceEngine
      engine.checkExhaustivity(tree)
      engine.checkRedundancy(tree)

      translated.ensureConforms(tree.tpe)
    }
}

object PatternMatcher {
  import ast.tpd._

  val name: String = "patternMatcher"

  final val selfCheck = false // debug option, if on we check that no case gets generated twice

  /** Minimal number of cases to emit a switch */
  final val MinSwitchCases = 4

  /** Was symbol generated by pattern matcher? */
  def isPatmatGenerated(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.is(Synthetic) && sym.name.is(PatMatStdBinderName)

  /** The pattern matching translator.
   *  Its general structure is a pipeline:
   *
   *     Match tree ---matchPlan---> Plan ---optimize---> Plan ---emit---> Tree
   *
   *  The pipeline consists of three steps:
   *
   *    - build a plan, using methods `matchPlan`, `caseDefPlan`, `patternPlan`.
   *    - optimize the plan, using methods listed in `optimization`,
   *    - emit the translated tree, using methods `emit`, `collectSwitchCases`,
   *      `emitSwitchCases`, and `emitCondition`.
   *
   *  A plan represents the underlying decision graph. It consists of tests,
   *  let bindings, labeled blocks, return from said labeled blocks and code blocks.
   *  It's represented by its own data type. Plans are optimized by merging common
   *  tests and eliminating dead code.
   */
  class Translator(resultType: Type, thisPhase: MiniPhase)(implicit ctx: Context) {

    // ------- Bindings for variables and labels ---------------------

    private val resultLabel =
      ctx.newSymbol(ctx.owner, PatMatResultName.fresh(), Synthetic | Label, resultType)

    /** A map from variable symbols to their defining trees
     *  and from labels to their defining plans
     */
    private val initializer = newMutableSymbolMap[Tree]

    private def newVar(rhs: Tree, flags: FlagSet): TermSymbol =
      ctx.newSymbol(ctx.owner, PatMatStdBinderName.fresh(), Synthetic | Case | flags,
        sanitize(rhs.tpe), coord = rhs.span)
        // TODO: Drop Case once we use everywhere else `isPatmatGenerated`.

    /** The plan `let x = rhs in body(x)` where `x` is a fresh variable */
    private def letAbstract(rhs: Tree)(body: Symbol => Plan): Plan = {
      val vble = newVar(rhs, EmptyFlags)
      initializer(vble) = rhs
      LetPlan(vble, body(vble))
    }

    /** The plan `l: { expr(l) }` where `l` is a fresh label */
    private def altsLabeledAbstract(expr: (=> Plan) => Plan): Plan = {
      val label = ctx.newSymbol(ctx.owner, PatMatAltsName.fresh(), Synthetic | Label,
        defn.UnitType)
      LabeledPlan(label, expr(ReturnPlan(label)))
    }

    /** Test whether a type refers to a pattern-generated variable */
    private val refersToInternal = new TypeAccumulator[Boolean] {
      def apply(x: Boolean, tp: Type) =
        x || {
          tp match {
            case tp: TermRef => isPatmatGenerated(tp.symbol)
            case _ => false
          }
        } || foldOver(x, tp)
    }

    /** Widen type as far as necessary so that it does not refer to a pattern-
     *  generated variable.
     */
    private def sanitize(tp: Type): Type = tp.widenIfUnstable match {
      case tp: TermRef if refersToInternal(false, tp) => sanitize(tp.underlying)
      case tp => tp
    }

    // ------- Plan and test types ------------------------

    /** Counter to display plans nicely, for debugging */
    private[this] var nxId = 0

    /** The different kinds of plans */
    sealed abstract class Plan { val id: Int = nxId; nxId += 1 }

    case class TestPlan(test: Test, var scrutinee: Tree, span: Span,
                        var onSuccess: Plan) extends Plan {
      override def equals(that: Any): Boolean = that match {
        case that: TestPlan => this.scrutinee === that.scrutinee && this.test == that.test
        case _ => false
      }
      override def hashCode: Int = scrutinee.hash * 41 + test.hashCode
    }

    case class LetPlan(sym: TermSymbol, var body: Plan) extends Plan
    case class LabeledPlan(sym: TermSymbol, var expr: Plan) extends Plan
    case class ReturnPlan(var label: TermSymbol) extends Plan
    case class SeqPlan(var head: Plan, var tail: Plan) extends Plan
    case class ResultPlan(var tree: Tree) extends Plan

    object TestPlan {
      def apply(test: Test, sym: Symbol, span: Span, ons: Plan): TestPlan =
        TestPlan(test, ref(sym), span, ons)
    }

    /** The different kinds of tests */
    sealed abstract class Test
    case class TypeTest(tpt: Tree) extends Test {                 // scrutinee.isInstanceOf[tpt]
      override def equals(that: Any): Boolean = that match {
        case that: TypeTest => this.tpt.tpe =:= that.tpt.tpe
        case _ => false
      }
      override def hashCode: Int = tpt.tpe.hash
    }
    case class EqualTest(tree: Tree) extends Test {               // scrutinee == tree
      override def equals(that: Any): Boolean = that match {
        case that: EqualTest => this.tree === that.tree
        case _ => false
      }
      override def hashCode: Int = tree.hash
    }
    case class LengthTest(len: Int, exact: Boolean) extends Test  // scrutinee (== | >=) len
    case object NonEmptyTest extends Test                         // !scrutinee.isEmpty
    case object NonNullTest extends Test                          // scrutinee ne null
    case object GuardTest extends Test                            // scrutinee

    // ------- Generating plans from trees ------------------------

    /** A set of variabes that are known to be not null */
    private val nonNull = mutable.Set[Symbol]()

    /** A conservative approximation of which patterns do not discern anything.
      * They are discarded during the translation.
      */
    private object WildcardPattern {
      def unapply(pat: Tree): Boolean = pat match {
        case Typed(_, tpt) if tpt.tpe.isRepeatedParam => true
        case Bind(nme.WILDCARD, WildcardPattern()) => true // don't skip when binding an interesting symbol!
        case t if isWildcardArg(t)                 => true
        case x: BackquotedIdent                    => false
        case x: Ident                              => x.name.isVariableName
        case Alternative(ps)                       => ps.forall(unapply)
        case EmptyTree                             => true
        case _                                     => false
      }
    }

    private object VarArgPattern {
      def unapply(pat: Tree): Option[Tree] = swapBind(pat) match {
        case Typed(pat1, tpt) if tpt.tpe.isRepeatedParam => Some(pat1)
        case _ => None
      }
    }

    /** Rewrite (repeatedly) `x @ (p: T)` to `(x @ p): T`
     *  This brings out the type tests to where they can be analyzed.
     */
    private def swapBind(tree: Tree): Tree = tree match {
      case Bind(name, pat0) =>
        swapBind(pat0) match {
          case Typed(pat, tpt) => Typed(cpy.Bind(tree)(name, pat), tpt)
          case _ => tree
        }
      case _ => tree
    }

    /** Plan for matching `scrutinee` symbol against `tree` pattern */
    private def patternPlan(scrutinee: Symbol, tree: Tree, onSuccess: Plan): Plan = {

      /** Plan for matching `selectors` against argument patterns `args` */
      def matchArgsPlan(selectors: List[Tree], args: List[Tree], onSuccess: Plan): Plan = {
        /* For a case with arguments that have some test on them such as
         * ```
         * case Foo(1, 2) => someCode
         * ```
         * all arguments values are extracted before the checks are performed. This shape is expected by `emit`
         * to avoid generating deep trees.
         * ```
         * val x1: Foo = ...
         * val x2: Int = x1._1
         * val x3: Int = x1._2
         * if (x2 == 1) {
         *   if (x3 == 2) someCode
         *   else ()
         * } else ()
         * ```
         */
        def matchArgsSelectorsPlan(selectors: List[Tree], syms: List[Symbol]): Plan =
          selectors match {
            case selector :: selectors1 => letAbstract(selector)(sym => matchArgsSelectorsPlan(selectors1, sym :: syms))
            case Nil => matchArgsPatternPlan(args, syms.reverse)
          }
        def matchArgsPatternPlan(args: List[Tree], syms: List[Symbol]): Plan =
          args match {
            case arg :: args1 =>
              val sym :: syms1 = syms
              patternPlan(sym, arg, matchArgsPatternPlan(args1, syms1))
            case Nil =>
              assert(syms.isEmpty)
              onSuccess
          }
        matchArgsSelectorsPlan(selectors, Nil)
      }

      /** Plan for matching the sequence in `seqSym` against sequence elements `args`.
       *  If `exact` is true, the sequence is not permitted to have any elements following `args`.
       */
      def matchElemsPlan(seqSym: Symbol, args: List[Tree], exact: Boolean, onSuccess: Plan) = {
        val selectors = args.indices.toList.map(idx =>
          ref(seqSym).select(defn.Seq_apply.matchingMember(seqSym.info)).appliedTo(Literal(Constant(idx))))
        TestPlan(LengthTest(args.length, exact), seqSym, seqSym.span,
          matchArgsPlan(selectors, args, onSuccess))
      }

      /** Plan for matching the sequence in `getResult` against sequence elements
       *  and a possible last varargs argument `args`.
       */
      def unapplySeqPlan(getResult: Symbol, args: List[Tree]): Plan = args.lastOption match {
        case Some(VarArgPattern(arg)) =>
          val matchRemaining =
            if (args.length == 1) {
              val toSeq = ref(getResult)
                .select(defn.Seq_toSeq.matchingMember(getResult.info))
              letAbstract(toSeq) { toSeqResult =>
                patternPlan(toSeqResult, arg, onSuccess)
              }
            }
            else {
              val dropped = ref(getResult)
                .select(defn.Seq_drop.matchingMember(getResult.info))
                .appliedTo(Literal(Constant(args.length - 1)))
              letAbstract(dropped) { droppedResult =>
                patternPlan(droppedResult, arg, onSuccess)
              }
            }
          matchElemsPlan(getResult, args.init, exact = false, matchRemaining)
        case _ =>
          matchElemsPlan(getResult, args, exact = true, onSuccess)
      }

      /** Plan for matching the result of an unapply against argument patterns `args` */
      def unapplyPlan(unapp: Tree, args: List[Tree]): Plan = {
        def caseClass = unapp.symbol.owner.linkedClass
        lazy val caseAccessors = caseClass.caseAccessors.filter(_.is(Method))

        def isSyntheticScala2Unapply(sym: Symbol) =
          sym.is(SyntheticCase) && sym.owner.is(Scala2x)

        if (isSyntheticScala2Unapply(unapp.symbol) && caseAccessors.length == args.length)
          matchArgsPlan(caseAccessors.map(ref(scrutinee).select(_)), args, onSuccess)
        else if (unapp.tpe.widenSingleton.isRef(defn.BooleanClass))
          TestPlan(GuardTest, unapp, unapp.span, onSuccess)
        else {
          letAbstract(unapp) { unappResult =>
            val isUnapplySeq = unapp.symbol.name == nme.unapplySeq
            if (isProductMatch(unapp.tpe.widen, args.length) && !isUnapplySeq) {
              val selectors = productSelectors(unapp.tpe).take(args.length)
                .map(ref(unappResult).select(_))
              matchArgsPlan(selectors, args, onSuccess)
            }
            else {
              assert(isGetMatch(unapp.tpe))
              val argsPlan = {
                val get = ref(unappResult).select(nme.get, _.info.isParameterless)
                if (isUnapplySeq)
                  letAbstract(get)(unapplySeqPlan(_, args))
                else
                  letAbstract(get) { getResult =>
                    val selectors =
                      if (args.tail.isEmpty) ref(getResult) :: Nil
                      else productSelectors(get.tpe).map(ref(getResult).select(_))
                    matchArgsPlan(selectors, args, onSuccess)
                  }
              }
              TestPlan(NonEmptyTest, unappResult, unapp.span, argsPlan)
            }
          }
        }
      }

      // begin patternPlan
      swapBind(tree) match {
        case Typed(pat, tpt) =>
          TestPlan(TypeTest(tpt), scrutinee, tree.span,
            letAbstract(ref(scrutinee).cast(tpt.tpe)) { casted =>
              nonNull += casted
              patternPlan(casted, pat, onSuccess)
            })
        case UnApply(extractor, implicits, args) =>
          val unappPlan = if (defn.isBottomType(scrutinee.info)) {
            // Generate a throwaway but type-correct plan.
            // This plan will never execute because it'll be guarded by a `NonNullTest`.
            ResultPlan(tpd.Throw(tpd.Literal(Constant(null))))
          } else {
            val mt @ MethodType(_) = extractor.tpe.widen
            var unapp = extractor.appliedTo(ref(scrutinee).ensureConforms(mt.paramInfos.head))
            if (implicits.nonEmpty) unapp = unapp.appliedToArgs(implicits)
            unapplyPlan(unapp, args)
          }
          if (scrutinee.info.isNotNull || nonNull(scrutinee)) unappPlan
          else TestPlan(NonNullTest, scrutinee, tree.span, unappPlan)
        case Bind(name, body) =>
          if (name == nme.WILDCARD) patternPlan(scrutinee, body, onSuccess)
          else {
            // The type of `name` may refer to val in `body`, therefore should come after `body`
            val bound = tree.symbol.asTerm
            initializer(bound) = ref(scrutinee)
            patternPlan(scrutinee, body, LetPlan(bound, onSuccess))
          }
        case Alternative(alts) =>
          altsLabeledAbstract { onf =>
            SeqPlan(
              altsLabeledAbstract { ons =>
                alts.foldRight(onf) { (alt, next) =>
                  SeqPlan(patternPlan(scrutinee, alt, ons), next)
                }
              },
              onSuccess
            )
          }
        case WildcardPattern() =>
          onSuccess
        case SeqLiteral(pats, _) =>
          matchElemsPlan(scrutinee, pats, exact = true, onSuccess)
        case _ =>
          TestPlan(EqualTest(tree), scrutinee, tree.span, onSuccess)
      }
    }

    private def caseDefPlan(scrutinee: Symbol, cdef: CaseDef): Plan = {
      var onSuccess: Plan = ResultPlan(cdef.body)
      if (!cdef.guard.isEmpty)
        onSuccess = TestPlan(GuardTest, cdef.guard, cdef.guard.span, onSuccess)
      patternPlan(scrutinee, cdef.pat, onSuccess)
    }

    private def matchPlan(tree: Match): Plan = {
      letAbstract(tree.selector) { scrutinee =>
        val matchError: Plan = ResultPlan(Throw(New(defn.MatchErrorType, ref(scrutinee) :: Nil)))
        tree.cases.foldRight(matchError) { (cdef, next) =>
          SeqPlan(caseDefPlan(scrutinee, cdef), next)
        }
      }
    }

    // ----- Optimizing plans ---------------

    /** A superclass for plan transforms */
    class PlanTransform extends (Plan => Plan) {
      protected val treeMap: TreeMap = new TreeMap {
        override def transform(tree: Tree)(implicit ctx: Context) = tree
      }
      def apply(tree: Tree): Tree = treeMap.transform(tree)
      def apply(plan: TestPlan): Plan = {
        plan.scrutinee = apply(plan.scrutinee)
        plan.onSuccess = apply(plan.onSuccess)
        plan
      }
      def apply(plan: LetPlan): Plan = {
        plan.body = apply(plan.body)
        initializer(plan.sym) = apply(initializer(plan.sym).nn)
        plan
      }
      def apply(plan: LabeledPlan): Plan = {
        plan.expr = apply(plan.expr)
        plan
      }
      def apply(plan: ReturnPlan): Plan = plan
      def apply(plan: SeqPlan): Plan = {
        plan.head = apply(plan.head)
        plan.tail = apply(plan.tail)
        plan
      }
      def apply(plan: Plan): Plan = plan match {
        case plan: TestPlan => apply(plan)
        case plan: LetPlan => apply(plan)
        case plan: LabeledPlan => apply(plan)
        case plan: ReturnPlan => apply(plan)
        case plan: SeqPlan => apply(plan)
        case plan: ResultPlan => plan
      }
    }

    private class RefCounter extends PlanTransform {
      val count = new mutable.HashMap[Symbol, Int] {
        override def default(key: Symbol) = 0
      }
    }

    /** Reference counts for all labels */
    private def labelRefCount(plan: Plan): collection.Map[Symbol, Int] = {
      object refCounter extends RefCounter {
        override def apply(plan: LabeledPlan): Plan = {
          apply(plan.expr)
          plan
        }
        override def apply(plan: ReturnPlan): Plan = {
          count(plan.label) += 1
          plan
        }
      }
      refCounter(plan)
      refCounter.count
    }

    /** Reference counts for all variables */
    private def varRefCount(plan: Plan): collection.Map[Symbol, Int] = {
      object refCounter extends RefCounter {
        override val treeMap = new TreeMap {
          override def transform(tree: Tree)(implicit ctx: Context) = tree match {
            case tree: Ident =>
              if (isPatmatGenerated(tree.symbol)) count(tree.symbol) += 1
              tree
            case _ =>
              super.transform(tree)
          }
        }
        override def apply(plan: LetPlan): Plan = {
          apply(plan.body)
          if (count(plan.sym) != 0 || !isPatmatGenerated(plan.sym))
            apply(initializer(plan.sym).nn)
          plan
        }
        override def apply(plan: SeqPlan): Plan = {
          apply(plan.head)
          if (canFallThrough(plan.head))
            apply(plan.tail)
          plan
        }
      }
      refCounter(plan)
      refCounter.count
    }

    /** Merge identical consecutive tests.
     *
     *  When we have the following shape:
     *
     *  if (testA) plan1
     *  if (testA) plan2
     *  nextPlan?
     *
     *  transform it to
     *
     *  if (testA) {
     *    plan1
     *    plan2
     *  }
     *  nextPlan?
     *
     *  Similarly, when we have equivalent let bindings:
     *
     *  let x1 = rhs1 in plan1
     *  let x2 = rhs2 in plan2
     *  nextPlan?
     *
     *  and rhs1 and rhs2 are equivalent, transform it to
     *
     *  let x1 = rhs1 in {
     *    plan1
     *    plan2[x1/x2]
     *  }
     *
     *  where plan2[x1/x2] means substituting x1 for x2 in plan2.
     *
     *  There are some tricks to "ignore" non-patmat-generated let bindings, i.e.,
     *  captures written in the source code, while identifying common subplans.
     */
    def mergeTests(plan: Plan): Plan = {
      class SubstituteIdent(from: TermSymbol, to: TermSymbol) extends PlanTransform {
        override val treeMap = new TreeMap {
          override def transform(tree: Tree)(implicit ctx: Context) = tree match {
            case tree: Ident if tree.symbol == from => ref(to)
            case _ => super.transform(tree)
          }
        }
      }

      class MergeTests extends PlanTransform {
        override def apply(plan: SeqPlan): Plan = {
          def tryMerge(plan1: Plan, plan2: Plan): Option[Plan] = {
            def skipNonPatmatGenedLets(plan: Plan): Plan = plan match {
              case LetPlan(sym, body) if !isPatmatGenerated(sym) =>
                skipNonPatmatGenedLets(body)
              case _ =>
                plan
            }

            def transferNonPatmatGenedLets(originalPlan: Plan, newPlan: Plan): Plan = originalPlan match {
              case originalPlan: LetPlan if !isPatmatGenerated(originalPlan.sym) =>
                originalPlan.body = transferNonPatmatGenedLets(originalPlan.body, newPlan)
                originalPlan
              case _ =>
                newPlan
            }

            (skipNonPatmatGenedLets(plan1), skipNonPatmatGenedLets(plan2)) match {
              case (testPlan1: TestPlan, testPlan2: TestPlan) if testPlan1 == testPlan2 =>
                /* Because testPlan2 is the same as testPlan1, it cannot possibly refer to
                 * the symbols defined by any of the skipped lets.
                 */
                testPlan1.onSuccess = SeqPlan(testPlan1.onSuccess,
                  transferNonPatmatGenedLets(plan2, testPlan2.onSuccess))
                Some(plan1) // that's the original plan1, on purpose

              case (letPlan1: LetPlan, letPlan2: LetPlan) if initializer(letPlan1.sym).nn === initializer(letPlan2.sym).nn =>
                // By construction, letPlan1.sym and letPlan2.sym are patmat-generated
                val newPlan2Body = new SubstituteIdent(letPlan2.sym, letPlan1.sym)(letPlan2.body)
                letPlan1.body = SeqPlan(letPlan1.body,
                  transferNonPatmatGenedLets(plan2, newPlan2Body))
                Some(plan1) // that's the original plan1, on purpose

              case _ =>
                None
            }
          }

          plan.head = apply(plan.head)
          plan.tail = apply(plan.tail)
          plan.tail match {
            case SeqPlan(tailHead, tailTail) =>
              tryMerge(plan.head, tailHead) match {
                case Some(merged) => SeqPlan(apply(merged), tailTail)
                case none => plan
              }
            case tail =>
              tryMerge(plan.head, tail) match {
                case Some(merged) => apply(merged)
                case none => plan
              }
          }
        }
      }
      new MergeTests()(plan)
    }

    /** Inline let-bound trees that are referenced only once and eliminate dead code.
     *
     *  - Drop all variables that are not referenced anymore after inlining.
     *  - Drop the `tail` of `SeqPlan`s whose `head` cannot fall through.
     */
    private def inlineVars(plan: Plan): Plan = {
      val refCount = varRefCount(plan)
      val LetPlan(topSym, _) = plan

      def toDrop(sym: Symbol) = initializer.get(sym) match {
        case Some(rhs) =>
          isPatmatGenerated(sym) && refCount(sym) <= 1 && sym != topSym && isPureExpr(rhs)
        case none =>
          false
      }

      object Inliner extends PlanTransform {
        override val treeMap = new TreeMap {
          override def transform(tree: Tree)(implicit ctx: Context) = tree match {
            case tree: Ident =>
              val sym = tree.symbol
              if (toDrop(sym)) transform(initializer(sym).nn)
              else tree
            case _ =>
              super.transform(tree)
          }
        }
        override def apply(plan: LetPlan): Plan = {
          if (toDrop(plan.sym)) apply(plan.body)
          else {
            initializer(plan.sym) = apply(initializer(plan.sym).nn)
            plan.body = apply(plan.body)
            plan
          }
        }
        override def apply(plan: SeqPlan): Plan = {
          val newHead = apply(plan.head)
          if (!canFallThrough(newHead)) {
            // If the head cannot fall through, the tail is dead code
            newHead
          }
          else {
            plan.head = newHead
            plan.tail = apply(plan.tail)
            plan
          }
        }
      }
      Inliner(plan)
    }

    // ----- Generating trees from plans ---------------

    /** The condition a test plan rewrites to */
    private def emitCondition(plan: TestPlan): Tree = {
      val scrutinee = plan.scrutinee
      (plan.test: @unchecked) match {
        case NonEmptyTest =>
          scrutinee
            .select(nme.isEmpty, _.info.isParameterless)
            .select(nme.UNARY_!, _.info.isParameterless)
        case NonNullTest =>
          scrutinee.testNotNull
        case GuardTest =>
          scrutinee
        case EqualTest(tree) =>
          tree.equal(scrutinee)
        case LengthTest(len, exact) =>
          val lengthCompareSym = defn.Seq_lengthCompare.matchingMember(scrutinee.tpe)
          if (lengthCompareSym.exists)
            scrutinee
              .select(defn.Seq_lengthCompare.matchingMember(scrutinee.tpe))
              .appliedTo(Literal(Constant(len)))
              .select(if (exact) defn.Int_== else defn.Int_>=)
              .appliedTo(Literal(Constant(0)))
          else // try length
            scrutinee
              .select(defn.Seq_length.matchingMember(scrutinee.tpe))
              .select(if (exact) defn.Int_== else defn.Int_>=)
              .appliedTo(Literal(Constant(len)))
        case TypeTest(tpt) =>
          val expectedTp = tpt.tpe

          // An outer test is needed in a situation like  `case x: y.Inner => ...`
          def outerTestNeeded: Boolean = {
            // See the test for SI-7214 for motivation for dealias. Later `treeCondStrategy#outerTest`
            // generates an outer test based on `patType.prefix` with automatically dealises.
            expectedTp.dealias match {
              case tref @ TypeRef(pre: SingletonType, _) =>
                tref.symbol.isClass &&
                ExplicitOuter.needsOuterIfReferenced(tref.symbol.asClass)
              case _ =>
                false
            }
          }

          def outerTest: Tree = thisPhase.transformFollowingDeep {
            val expectedOuter = singleton(expectedTp.normalizedPrefix)
            val expectedClass = expectedTp.dealias.classSymbol.asClass
            ExplicitOuter.ensureOuterAccessors(expectedClass)
            scrutinee.ensureConforms(expectedTp)
              .outerSelect(1, expectedClass.owner.typeRef)
              .select(defn.Object_eq)
              .appliedTo(expectedOuter)
          }

          expectedTp.dealias match {
            case expectedTp: SingletonType =>
              scrutinee.isInstance(expectedTp)  // will be translated to an equality test
            case _ =>
              val typeTest = scrutinee.select(defn.Any_typeTest).appliedToType(expectedTp)
              if (outerTestNeeded) typeTest.and(outerTest) else typeTest
          }
      }
    }

    @tailrec
    private def canFallThrough(plan: Plan): Boolean = plan match {
      case _:ReturnPlan | _:ResultPlan => false
      case _:TestPlan | _:LabeledPlan => true
      case LetPlan(_, body) => canFallThrough(body)
      case SeqPlan(_, tail) => canFallThrough(tail)
    }

    /** Collect longest list of plans that represent possible cases of
     *  a switch, including a last default case, by starting with this
     *  plan and following onSuccess plans.
     */
    private def collectSwitchCases(scrutinee: Tree, plan: SeqPlan): List[(List[Tree], Plan)] = {
      def isSwitchableType(tpe: Type): Boolean =
        (tpe isRef defn.IntClass) ||
        (tpe isRef defn.ByteClass) ||
        (tpe isRef defn.ShortClass) ||
        (tpe isRef defn.CharClass)

      val seen = mutable.Set[Int]()

      def isNewIntConst(tree: Tree) = tree match {
        case Literal(const) if const.isIntRange && !seen.contains(const.intValue) =>
          seen += const.intValue
          true
        case _ =>
          false
      }

      // An extractor to recover the shape of plans that can become alternatives
      object AlternativesPlan {
        def unapply(plan: LabeledPlan): Option[(List[Tree], Plan)] = {
          plan.expr match {
            case SeqPlan(LabeledPlan(innerLabel, innerPlan), ons) if !canFallThrough(ons) =>
              val outerLabel = plan.sym
              val alts = List.newBuilder[Tree]
              def rec(innerPlan: Plan): Boolean = innerPlan match {
                case SeqPlan(TestPlan(EqualTest(tree), scrut, _, ReturnPlan(`innerLabel`)), tail)
                if scrut === scrutinee && isNewIntConst(tree) =>
                  alts += tree
                  rec(tail)
                case ReturnPlan(`outerLabel`) =>
                  true
                case _ =>
                  false
              }
              if (rec(innerPlan))
                Some((alts.result(), ons))
              else
                None

            case _ =>
              None
          }
        }
      }

      def recur(plan: Plan): List[(List[Tree], Plan)] = plan match {
        case SeqPlan(testPlan @ TestPlan(EqualTest(tree), scrut, _, ons), tail)
        if scrut === scrutinee && !canFallThrough(ons) && isNewIntConst(tree) =>
          (tree :: Nil, ons) :: recur(tail)
        case SeqPlan(AlternativesPlan(alts, ons), tail) =>
          (alts, ons) :: recur(tail)
        case _ =>
          (Nil, plan) :: Nil
      }

      if (isSwitchableType(scrutinee.tpe.widen)) recur(plan)
      else Nil
    }

    private def hasEnoughSwitchCases(cases: List[(List[Tree], Plan)], required: Int): Boolean = {
      // 1 because of the default case
      required <= 1 || {
        cases match {
          case (alts, _) :: cases1 => hasEnoughSwitchCases(cases1, required - alts.size)
          case _ => false
        }
      }
    }

    /** Emit cases of a switch */
    private def emitSwitchCases(cases: List[(List[Tree], Plan)]): List[CaseDef] = (cases: @unchecked) match {
      case (alts, ons) :: cases1 =>
        val pat = alts match {
          case alt :: Nil => alt
          case Nil => Underscore(defn.IntType) // default case
          case _ => Alternative(alts)
        }
        CaseDef(pat, EmptyTree, emit(ons)) :: emitSwitchCases(cases1)
      case nil =>
        Nil
    }

    /** If selfCheck is `true`, used to check whether a tree gets generated twice */
    private val emitted = mutable.Set[Int]()

    /** Translate plan to tree */
    private def emit(plan: Plan): Tree = {
      if (selfCheck) {
        assert(plan.isInstanceOf[ReturnPlan] || !emitted.contains(plan.id), plan.id)
        emitted += plan.id
      }
      plan match {
        case plan: TestPlan =>
          /** Merge nested `if`s that have the same `else` branch into a single `if`.
           *  This optimization targets calls to label defs for case failure jumps to next case.
           *
           *  Plan for
           *  ```
           *  val x1: Int = ...
           *  val x2: Int = ...
           *  if (x1 == y1) {
           *    if (x2 == y2) someCode
           *    else ()
           *  } else ()
           *  ```
           *  is emitted as
           *  ```
           *  val x1: Int = ...
           *  val x2: Int = ...
           *  if (x1 == y1 && x2 == y2) someCode
           *  else ()
           *  ```
           */
          def emitWithMashedConditions(plans: List[TestPlan]): Tree = {
            val plan = plans.head
            plan.onSuccess match {
              case plan2: TestPlan =>
                emitWithMashedConditions(plan2 :: plans)
              case _ =>
                def emitCondWithPos(plan: TestPlan) = emitCondition(plan).withSpan(plan.span)
                val conditions =
                  plans.foldRight[Tree](EmptyTree) { (otherPlan, acc) =>
                    if (acc.isEmpty) emitCondWithPos(otherPlan)
                    else acc.select(nme.ZAND).appliedTo(emitCondWithPos(otherPlan))
                  }
                If(conditions, emit(plan.onSuccess), unitLiteral)
            }
          }
          emitWithMashedConditions(plan :: Nil)
        case LetPlan(sym, body) =>
          seq(ValDef(sym, initializer(sym).nn.ensureConforms(sym.info)) :: Nil, emit(body))
        case LabeledPlan(label, expr) =>
          Labeled(label, emit(expr))
        case ReturnPlan(label) =>
          Return(Literal(Constant(())), ref(label))
        case plan: SeqPlan =>
          def default = seq(emit(plan.head) :: Nil, emit(plan.tail))
          def maybeEmitSwitch(scrutinee: Tree): Tree = {
            val switchCases = collectSwitchCases(scrutinee, plan)
            if (hasEnoughSwitchCases(switchCases, MinSwitchCases)) // at least 3 cases + default
              Match(scrutinee, emitSwitchCases(switchCases))
            else
              default
          }
          plan.head match {
            case testPlan: TestPlan =>
              maybeEmitSwitch(testPlan.scrutinee)
            case LabeledPlan(_, SeqPlan(LabeledPlan(_, SeqPlan(testPlan: TestPlan, _)), _)) =>
              maybeEmitSwitch(testPlan.scrutinee)
            case _ =>
              default
          }
        case ResultPlan(tree) =>
          if (tree.tpe <:< defn.NothingType) tree // For example MatchError
          else Return(tree, ref(resultLabel))
      }
    }

    /** Pretty-print plan; used for debugging */
    def show(plan: Plan): String = {
      val lrefCount = labelRefCount(plan)
      val vrefCount = varRefCount(plan)
      val sb = new StringBuilder
      val seen = mutable.Set[Int]()
      def showTest(test: Test) = test match {
        case EqualTest(tree) => i"EqualTest($tree)"
        case TypeTest(tpt) => i"TypeTest($tpt)"
        case _ => test.toString
      }
      def showPlan(plan: Plan): Unit =
        if (!seen.contains(plan.id)) {
          seen += plan.id
          sb append s"\n${plan.id}: "
          plan match {
            case TestPlan(test, scrutinee, _, ons) =>
              sb.append(i"$scrutinee ? ${showTest(test)}(${ons.id})")
              showPlan(ons)
            case LetPlan(sym, body) =>
              sb.append(i"Let($sym = ${initializer(sym)}}, ${body.id})")
              sb.append(s", refcount = ${vrefCount(sym)}")
              showPlan(body)
            case LabeledPlan(label, expr) =>
              sb.append(i"Labeled($label: { ${expr.id} })")
              sb.append(s", refcount = ${lrefCount(label)}")
              showPlan(expr)
            case ReturnPlan(label) =>
              sb.append(s"Return($label)")
            case SeqPlan(head, tail) =>
              sb.append(s"Seq(${head.id}, ${tail.id})")
              showPlan(head)
              showPlan(tail)
            case ResultPlan(tree) =>
              sb.append(tree.show)
          }
        }
      showPlan(plan)
      sb.toString
    }

    /** If match is switch annotated, check that it translates to a switch
     *  with at least as many cases as the original match.
     */
    private def checkSwitch(original: Match, result: Tree) = original.selector match {
      case Typed(_, tpt) if tpt.tpe.hasAnnotation(defn.SwitchAnnot) =>
        val resultCases = result match {
          case Match(_, cases) => cases
          case Block(_, Match(_, cases)) => cases
          case _ => Nil
        }
        def typesInPattern(pat: Tree): List[Type] = pat match {
          case Alternative(pats) => pats.flatMap(typesInPattern)
          case _ => pat.tpe :: Nil
        }
        def typesInCases(cdefs: List[CaseDef]): List[Type] =
          cdefs.flatMap(cdef => typesInPattern(cdef.pat))
        def numTypes(cdefs: List[CaseDef]): Int =
          typesInCases(cdefs).toSet.size: Int // without the type ascription, testPickling fails because of #2840.
        if (numTypes(resultCases) < numTypes(original.cases)) {
          patmatch.println(i"switch warning for ${ctx.compilationUnit}")
          patmatch.println(i"original types: ${typesInCases(original.cases)}%, %")
          patmatch.println(i"switch types  : ${typesInCases(resultCases)}%, %")
          patmatch.println(i"tree = $result")
          ctx.warning(UnableToEmitSwitch(numTypes(original.cases) < MinSwitchCases), original.sourcePos)
        }
      case _ =>
    }

    val optimizations: List[(String, Plan => Plan)] = List(
      "mergeTests" -> mergeTests,
      "inlineVars" -> inlineVars
    )

    /** Translate pattern match to sequence of tests. */
    def translateMatch(tree: Match): Tree = {
      var plan = matchPlan(tree)
      patmatch.println(i"Plan for $tree: ${show(plan)}")
      if (!ctx.settings.YnoPatmatOpt.value)
        for ((title, optimization) <- optimizations) {
          plan = optimization(plan)
          patmatch.println(s"After $title: ${show(plan)}")
        }
      val result = emit(plan)
      checkSwitch(tree, result)
      Labeled(resultLabel, result)
    }
  }
}
