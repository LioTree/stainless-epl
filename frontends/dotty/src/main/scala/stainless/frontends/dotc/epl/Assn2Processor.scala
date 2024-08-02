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
import stainless.equivchkplus.{optAssn2, optExtractTarget}

class Assn2Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends EPLProcessor {

  import ast.untpd.*

  private val splitFunctions = List("eval", "tyOf", "subst", "desugar")
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

    def getSubFuns(using dottyCtx: DottyContext): List[DefDef] = {
      traverse(baseFun)
      subFuns
    }

    def markSubFun(subFun: DefDef, markName: String): DefDef = {
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
          val newParamss = baseFun.paramss.map(_.map {
            case valDef@ValDef(name, tpt, _) if name.toString == baseMatch.selector.asInstanceOf[Ident].name.toString =>
              cpy.ValDef(valDef)(name, Ident(fun.name.toTypeName), valDef.rhs)
            case other => other
          }.asInstanceOf[ParamClause])
          val newRhs = cpy.Match(baseMatch)(baseMatch.selector, List(cpy.CaseDef(tree)(pat, EmptyTree, body)))
          val subFun = recCallRewriter.transform(cpy.DefDef(baseFun)(newName, newParamss, baseFun.tpt, newRhs)).asInstanceOf[DefDef]
          subFuns = markSubFun(subFun, fun.name.toString) :: subFuns
        }

//        case CaseDef(pat@Tuple(List(Apply(fun1: Ident, args1), Apply(fun2: Ident, args2))), EmptyTree, body) => {
//          val newName = termName(s"${baseFun.name.toString}_${fun1.name.toTermName}_${fun2.name.toTermName}")
//          val newRhs = cpy.Match(baseMatch)(baseMatch.selector, List(cpy.CaseDef(tree)(pat, EmptyTree, body)))
//          val subFun = recCallRewriter.transform(cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRhs)).asInstanceOf[DefDef]
//          subFuns = markSubFun(subFun, s"${fun1.name.toString}_${fun2.name.toString}") :: subFuns
//        }

        case CaseDef(Ident(name), EmptyTree, body) if name.toString == "_" =>

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

      case _ => super.transform(tree)
    }
}
