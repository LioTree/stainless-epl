package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.Spans.Span
import inox.OptionValue
import stainless.equivchk.optSubFnsEquiv
import stainless.epl.{optAssn2, optFakeExercises, optExtractTarget, optGenSubFuns}
import scala.collection.immutable.Set

class Assn2Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends PackageNameRewriter {

  import ast.untpd.*

  private val targets = inoxCtx.options.findOption(optExtractTarget) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }

  private val framework = Set(
    "Variable",
    "Env",
    "Expr",
    "Num",
    "Plus",
    "Minus",
    "Times",
    "Bool",
    "Eq",
    "IfThenElse",
    "Str",
    "Length",
    "Index",
    "Concat",
    "Var",
    "Let",
    "LetFun",
    "LetRec",
    "LetPair",
    "Pair",
    "First",
    "Second",
    "Lambda",
    "Apply",
    "Rec",
    "Value",
    "NumV",
    "BoolV",
    "StringV",
    "PairV",
    "ClosureV",
    "RecV",
    "Type",
    "IntTy",
    "BoolTy",
    "StringTy",
    "PairTy",
    "FunTy",
    "Gensym",
    "swapVar",
    "swap"
  )

  // Exercises requiring the generation of sub-functions for separate verification.
  private val hardExercises = Set("eval", "tyOf", "subst", "desugar")
  private val isHardEx = hardExercises.intersect(targets).nonEmpty
  inoxCtx.options.findOption(optSubFnsEquiv) match {
    case Some(true) => inoxCtx.options + OptionValue(optGenSubFuns)(true)
    case _ =>
  }
  private val splitFuns = inoxCtx.options.findOption(optGenSubFuns) match {
    case Some(true) => hardExercises
    case _ => Set.empty
  }

  private val fakeExercises = inoxCtx.options.findOption(optFakeExercises) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }
  private val fakeCallPrefix = "fake_"

  private val unsafeMap = Set("ctx", "env")

  private class SubFunctionGenerator(baseFun: DefDef) extends UntypedTreeTraverser {

    private class RecCallRewriter extends ast.untpd.UntypedTreeMap {
      override def transform(tree: Tree)(using DottyContext): Tree =
        tree match {
          case Apply(Ident(name), args) if name.toString == baseFun.name.toString =>
            cpy.Apply(tree)(termIdent(fakeCallPrefix + name.toString), transform(args))
          case Apply(Select(Ident(name1), name2), args) if s"${name1.toString}.${name2.toString}" == "Gensym.gensym" =>
            Block(List(InfixOp(termIdent("freshSym"), termIdent("+="), buildNumber(1))), termIdent("freshSym"))
          case _ => super.transform(tree)
        }
    }

    private var baseMatch: Match = _
    private var subFuns = List.empty[DefDef]
    private val recCallRewriter = new RecCallRewriter
    private var defaultSubFun = false

    def getSubFuns(using dottyCtx: DottyContext): List[DefDef] = {
      traverse(baseFun)
      // try to generate the default sub-function if it is not generated
      genDefaultSubFn(errorWrapper)
      subFuns
    }

    private def markSubFun(subFun: DefDef, markName: String): DefDef = {
      val spanStart = subFun.span.start

      val subFnIdent = typeIdent("subFn")
      // A very necessary step, otherwise errors will occur in the typer.
      subFnIdent.span = Span(spanStart, spanStart + 6)
      val fileName = extractFileName(dottyCtx.source.toString)
      val subFnAnnotation = Apply(Select(New(subFnIdent), termName("<init>")),
        List(Literal(Constants.Constant(s"${fileName}.${fileName}$$package.${baseFun.name.toString}")),
          Literal(Constants.Constant(markName))))

      subFun.withAnnotations(List(subFnAnnotation))
    }

    private def genDefaultSubFn(body: untpd.Tree): Unit = {
      def markDefaultSubFun(subFun: DefDef, markName: String): DefDef = {
        val spanStart = subFun.span.start

        val subFnIdent = typeIdent("subFn")
        // A very necessary step, otherwise errors will occur in the typer.
        subFnIdent.span = Span(spanStart, spanStart + 6)
        val fileName = extractFileName(dottyCtx.source.toString)
        val subFnAnnotation = Apply(Select(New(subFnIdent), termName("<init>")),
          List(Literal(Constants.Constant(s"${fileName}.${fileName}$$package.${baseFun.name.toString}")),
            Literal(Constants.Constant(markName))))

        val defaultSubFnIdent = typeIdent("defaultSubFn")
        defaultSubFnIdent.span = Span(spanStart, spanStart + 14)
        val defaultSubFnAnnotation = Apply(Select(New(defaultSubFnIdent), termName("<init>")), Nil)

        subFun.withAnnotations(List(subFnAnnotation, defaultSubFnAnnotation))
      }

      // Only one default sub-function is allowed
      if (!defaultSubFun) {
        defaultSubFun = true
        // Default case, whatever the name it is, we just use a "_" as its name
        val newName = termName(s"${baseFun.name.toString}__")
        val newRhs = cpy.Match(baseMatch)(baseMatch.selector, List(CaseDef(Ident(termName("_")), EmptyTree, body)))
        val subFun = recCallRewriter.transform(cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRhs)).asInstanceOf[DefDef]
        subFuns = markDefaultSubFun(subFun, "_") :: subFuns
      }
    }

    override def traverse(tree: untpd.Tree)(using dottyCtx: DottyContext): Unit =
      tree match {
        case defDef@DefDef(name, paramss, tpt, _) =>
          defDef.rhs match
            case _match@Match(selector@Ident(selectorName), cases) =>
              if (baseMatch == null) baseMatch = _match
              traverseChildren(_match)
            case _ => sys.error("Invalid Assn2")

        case CaseDef(pat@Apply(fun: Ident, args), EmptyTree, body) => {
          val newName = termName(s"${baseFun.name.toString}_${fun.name.toTermName}")
          val freshSymDef = ValDef(termName("freshSym"), TypeTree(), Apply(termIdent("BigInt"), List(buildNumber(0))))
          freshSymDef.setMods(Modifiers(Flags.Mutable))
          val defaultCase = CaseDef(termIdent("_"), EmptyTree, errorWrapper) // pass match exhaustiveness verification.
          val newRhs = Block(List(freshSymDef), cpy.Match(baseMatch)(baseMatch.selector, List(cpy.CaseDef(tree)(pat, EmptyTree, body), defaultCase)))
          val subFun = recCallRewriter.transform(cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRhs)).asInstanceOf[DefDef]
          subFuns = markSubFun(subFun, fun.name.toString) :: subFuns
        }

        // If there is a default branch, use its body to generate the default sub-function.
        case CaseDef(Ident(name), EmptyTree, body) =>
          genDefaultSubFn(body)

        case CaseDef(_, _, _) => sys.error("Invalid Assn2")

        case _ => traverseChildren(tree)
      }
  }

  override def start(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    inoxCtx.options.findOption(optAssn2) match {
      case Some(true) => transform(tree)
      case _ => tree
    }


  private def genFakeFun(baseFun: DefDef): DefDef = {
    val newName = termName(fakeCallPrefix + baseFun.name.toString)
    val newRes = errorWrapper
    val fakeFun = cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRes)
    markExternPure(fakeFun).asInstanceOf[DefDef]
  }

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    tree match {
      case PackageDef(pid: Ident, stats) if pid.name.toString == "<empty>" => {
        var subFunctions = List.empty[DefDef]
        val importFramework = {
          // Assn2 exercises1 requires retaining the StringWrapper in the framework code.
          if (targets.contains("Value"))
            buildImport("epl.assn2.framework.Exercise1._")
          // The other exercises will use the BigInt version of the framework code.
          else
            buildImport("epl.assn2.framework.Others._")
        }
        // Specify the use of a dependent fake exercise. For example, exercise5 depends on exercise4, so you need to introduce fake.subst
        val importFakeExs = fakeExercises.map { exer =>
          buildImport(s"epl.assn2.fake.${exer}")
        }.toList
        // For the hard exercise in the target, it is necessary to introduce fake_xx to eliminate recursive calls.
        val importFakeCalls = targets.intersect(splitFuns).map { target =>
          buildImport(s"epl.assn2.fake.${fakeCallPrefix}${target}")
        }.toList

        val newStats = importFramework :: importFakeExs ++ importFakeCalls ++ stats.flatMap {
          // remove the original framework and specific exercises
          case TypeDef(name, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          case DefDef(name, _, _, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          // Since `Exercise1` is also a `Value`, it will be handled specially.
          case ModuleDef(name, _) if (name.toString != "Value" && framework.contains(name.toString)) || fakeExercises.contains(name.toString) =>
            Nil
          case ValDef(name, _, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil

          case defDef@DefDef(name, paramss, tpt, _) if splitFuns.contains(name.toString) =>
            subFunctions = subFunctions ++ (new SubFunctionGenerator(defDef)).getSubFuns
            List(markExternPure(defDef))

          case other => List(other)
        } ++ subFunctions

        super.transform(cpy.PackageDef(tree)(pid, newStats))
      }

      case Apply(Ident(name), args) if unsafeMap.contains(name.toString) =>
        Apply(Select(Ident(name), termName("getOrElse")), args :+ errorWrapper)

      case Apply(Select(Ident(name), name2), args) if (name2.toString == "apply") && unsafeMap.contains(name.toString) =>
        Apply(Select(Ident(name), termName("getOrElse")), args :+ errorWrapper)

      case untpd.AppliedTypeTree(tpt: Ident, List(arg: Ident)) if tpt.name.toString == "Env" && arg.name.toString == "Value" =>
        cpy.AppliedTypeTree(tree)(tpt, List(typeIdent("FakeValue")))

      // Not sure whether we should put this into pure scala translator.
      // Stainless does not support custom == equals, which may cause semantic inconsistencies.
      case Apply(Select(qualifier, name), List(arg)) if name.toString == "equals" || name.toString == "eq" =>
        InfixOp(transform(qualifier), termIdent("=="), transform(arg))

      // Only BigInt in Assn2 since OverflowInt might lead to extra performance overhead.
      case Apply(Ident(name), List(Apply(Ident(name2), List(num:Number)))) if name == termName("OverflowInt") && name2 == termName("BigInt") =>
        Apply(Ident(termName("BigInt")), List(transform(num)))

      case Ident(name) if name == typeName("OverflowInt") =>
        typeIdent("BigInt")

      // No String in Assn2 exercises 2-5 since it will make verification really hard.
      case Ident(name) if isHardEx && name.toString == "StringWrapper" =>
        name match {
          case name if name.isTermName => termIdent("BigInt")
          case name if name.isTypeName => typeIdent("BigInt")
        }

      // transform string and char literals to numbers
      case Literal(constant: Constants.Constant) if isHardEx && constant.value.isInstanceOf[Character] =>
        buildNumber(Utils.str2Int(constant.value.asInstanceOf[Character].toString))

      case Literal(constant: Constants.Constant) if isHardEx && constant.value.isInstanceOf[String] =>
        buildNumber(Utils.str2Int(constant.value.asInstanceOf[String]))

      case Apply(Ident(name), args) if name.toString == "swap" =>
        cpy.Apply(tree)(termIdent("eplSwap"), transform(args))

      case _ => super.transform(tree)
    }
}
