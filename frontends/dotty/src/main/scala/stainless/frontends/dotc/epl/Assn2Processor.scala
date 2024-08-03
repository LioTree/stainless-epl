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
import stainless.equivchkplus.{optAssn2, optExtractTarget, optSubFnsEquiv}

class Assn2Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends EPLProcessor {

  import ast.untpd.*

  private val splitFunctions = inoxCtx.options.findOption(optSubFnsEquiv) match {
    case Some(true) =>
      List("eval", "tyOf", "subst", "desugar")
    case _ => Nil
  }
  private val safeListMap = List("ctx", "env")
  private val fakePrefix = "fake_"

  private class SubFunctionGenerator(baseFun: DefDef) extends UntypedTreeTraverser {

    private class RecCallRewriter extends ast.untpd.UntypedTreeMap {
      override def transform(tree: Tree)(using DottyContext): Tree =
        tree match {
          case Apply(Ident(name), args) if name.toString == baseFun.name.toString =>
            cpy.Apply(tree)(Ident(termName(fakePrefix + name.toString)), transform(args))
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
      genDefaultSubFn(TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing")))))
      subFuns
    }

    private def markSubFun(subFun: DefDef, markName: String): DefDef = {
      val spanStart = subFun.span.start

      val subFnIdent = Ident(typeName("subFn"))
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

        val subFnIdent = Ident(typeName("subFn"))
        // A very necessary step, otherwise errors will occur in the typer.
        subFnIdent.span = Span(spanStart, spanStart + 6)
        val fileName = extractFileName(dottyCtx.source.toString)
        val subFnAnnotation = Apply(Select(New(subFnIdent), termName("<init>")),
          List(Literal(Constants.Constant(s"${fileName}.${fileName}$$package.${baseFun.name.toString}")),
            Literal(Constants.Constant(markName))))

        val defaultSubFnIdent = Ident(typeName("defaultSubFn"))
        defaultSubFnIdent.span = Span(spanStart, spanStart + 14)
        val defaultSubFnAnnotation = Apply(Select(New(defaultSubFnIdent), termName("<init>")), Nil)

        subFun.withAnnotations(List(subFnAnnotation, defaultSubFnAnnotation))
      }

      // Only one default sub-function is allowed
      if(!defaultSubFun) {
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

        case CaseDef(pat@Apply(fun: Ident, args), EmptyTree, body) if fun.name.toString == "Apply" || fun.name.toString == "IfThenElse" =>

        case CaseDef(pat@Apply(fun: Ident, args), EmptyTree, body) => {
          val newName = termName(s"${baseFun.name.toString}_${fun.name.toTermName}")
          val newRhs = cpy.Match(baseMatch)(baseMatch.selector, List(cpy.CaseDef(tree)(pat, EmptyTree, body)))
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

  def genFakeFun(baseFun: DefDef): DefDef = {
    val newName = termName(fakePrefix + baseFun.name.toString)
    val newRes = TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing"))))
    val fakeFun = cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRes)
    markExternPure(fakeFun).asInstanceOf[DefDef]
  }

  override def start(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    inoxCtx.options.findOption(optAssn2) match {
      case Some(true) => transform(tree)
      case _ => tree
    }

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    tree match {
      case PackageDef(pid: Ident, stats) if pid.name.toString == "<empty>" => {
        var subFunctions = List.empty[DefDef]
        var fakeFunctions = List.empty[DefDef]

        val newStats = stats.flatMap {
          // Replace Map with ListMap to avoid https://github.com/epfl-lara/stainless/issues/1547
          case typeDef@TypeDef(name, rhs@LambdaTypeTree(tparams, body: AppliedTypeTree)) if name.toString == "Env" =>
            val newBody = cpy.AppliedTypeTree(body)(Ident(typeName("ListMap")), transform(body.args))
            List(cpy.TypeDef(typeDef)(name, cpy.LambdaTypeTree(rhs)(transformSub(tparams), transform(newBody))))

          case moduleDef@ModuleDef(name, impl) if name.toString == "Gensym" =>
            List(markExternPure(untpd.cpy.ModuleDef(moduleDef)(name, transformSub(impl))))

          case defDef@DefDef(name, paramss, tpt, _) if splitFunctions.contains(name.toString) =>
            subFunctions = subFunctions ++ (new SubFunctionGenerator(defDef)).getSubFuns
            fakeFunctions = fakeFunctions :+ genFakeFun(defDef)
            List(transform(defDef))

          case other => List(transform(other))
        } ++ fakeFunctions ++ subFunctions

        super.transform(cpy.PackageDef(tree)(pid, newStats))
      }

      case Apply(Ident(name), args) if safeListMap.contains(name.toString) =>
        Apply(Select(Ident(name), termName("getOrElse")), args :+ TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing")))))

      case Apply(Select(Ident(name),name2), args) if (name2.toString == "apply" || name2.toString == "get") && safeListMap.contains(name.toString) =>
        Apply(Select(Ident(name), termName("getOrElse")), args :+ TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing")))))

      case Apply(Select(qualifier, name), args) if name.toString == "equals" || name.toString == "eq" =>
        InfixOp(transform(qualifier), Ident(termName("==")), transform(args(0)))

      case _ => super.transform(tree)
    }
}
