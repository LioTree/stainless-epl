package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.equivchkplus.optAssn2

class Assn2Preprocessor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends AssnProcessor {

  import ast.untpd.*

  private class SubFunGenerator(baseFun: DefDef)(using dottyCtx: DottyContext) extends UntypedTreeTraverser {

    private class RecCallRewriter extends ast.untpd.UntypedTreeMap {
      private val prefix = "fake_"

      override def transform(tree: Tree)(using DottyContext): Tree =
        tree match {
          case Apply(Ident(name), args) if name.toString == baseFun.name.toString =>
            cpy.Apply(tree)(Ident(termName(prefix + name.toString)), args)
          case _ => super.transform(tree)
        }
    }

    private var baseMatch: Match = _
    private var subFuns = List.empty[DefDef]
    private val recCallRewriter = new RecCallRewriter

    def getSubFuns: List[DefDef] = {
      traverse(baseFun)
      subFuns
    }

    override def traverse(tree: untpd.Tree)(using dottyCtx: DottyContext): Unit =
      tree match {
        case defDef@DefDef(name, paramss, tpt, _) =>
          defDef.rhs match
            case _match@Match(selector@Ident(selectorName), cases) =>
              if (baseMatch == null) baseMatch = _match
              traverseChildren(_match)
            case _ => sys.error("Invalid Assn2")

        case CaseDef(pat@Apply(fun: Ident, args), EmptyTree, body) =>
          val newName = termName(s"${baseFun.name.toString}_${fun.name.toTermName}")
          val newParamss = baseFun.paramss.map(_.map {
            case valDef@ValDef(name, tpt, _) if name.toString == baseMatch.selector.asInstanceOf[Ident].name.toString =>
              cpy.ValDef(valDef)(name, Ident(fun.name.toTypeName), valDef.rhs)
            case other => other
          }.asInstanceOf[ParamClause])
          val newRhs = cpy.Match(baseMatch)(baseMatch.selector, List(cpy.CaseDef(tree)(pat, EmptyTree, body)))
          val subFun = recCallRewriter.transform(cpy.DefDef(baseFun)(newName, newParamss, baseFun.tpt, newRhs)).asInstanceOf[DefDef]
          subFuns = subFun :: subFuns

        case CaseDef(Ident(name), EmptyTree, body) if name.toString == "_" =>

        case CaseDef(_, _, _) => sys.error("Invalid Assn2")

        case _ => traverseChildren(tree)
      }
  }

  def genFakeFun(baseFun: DefDef): DefDef = {
    val prefix = "fake_"
    val newName = termName(prefix + baseFun.name.toString)
    val newRes = TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing"))))
    val fakeFun = cpy.DefDef(baseFun)(newName, baseFun.paramss, baseFun.tpt, newRes)
    markExternPure(fakeFun).asInstanceOf[DefDef]
  }

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree = {
    inoxCtx.options.findOption(optAssn2) match
      case Some(true) =>
        tree match {
          case PackageDef(pid: Ident, stats) =>
            val newStats = stats.flatMap(
              stat => stat match {

                case _import: Import if _import.show == "import scala.collection.immutable.Set" =>
                  List(EmptyTree)

                case _import: Import if _import.show == "import scala.util.parsing.combinator.PackratParsers" =>
                  List(EmptyTree)

                case _import: Import if _import.show == "import scala.util.parsing.combinator.syntactical.StandardTokenParsers" =>
                  List(EmptyTree)

                case ModuleDef(name, impl) if name.toString == "Assn2" =>
                  var subFunctions = List.empty[DefDef]
                  var fakeFunctions = List.empty[DefDef]

                  impl.body.filterNot {
                    case DefDef(name, _, _, _) if name.toString == "example1" => true
                    case DefDef(name, _, _, _) if name.toString == "example2" => true
                    case DefDef(name, _, _, _) if name.toString == "example3" => true
                    case DefDef(name, _, _, _) if name.toString == "example4" => true
                    case DefDef(name, _, _, _) if name.toString == "example5" => true
                    case DefDef(name, _, _, _) if name.toString == "example6" => true
                    case TypeDef(name, _) if name.toString == "GiraffeParser" => true
                    case ValDef(name, _, _) if name.toString == "parser" => true
                    case ModuleDef(name, _) if name.toString == "Main" => true
                    case DefDef(name, _, _, _) if name.toString == "main" => true
                    case _ => false
                  }.map {
                    // Replace Map with ListMap to avoid https://github.com/epfl-lara/stainless/issues/1547
                    case typeDef@TypeDef(name, rhs@LambdaTypeTree(tparams, body: AppliedTypeTree)) if name.toString == "Env" =>
                      val newBody = cpy.AppliedTypeTree(body)(Ident(typeName("ListMap")), super.transform(body.args))
                      cpy.TypeDef(typeDef)(name, cpy.LambdaTypeTree(rhs)(transformSub(tparams), super.transform(newBody)))

                    case moduleDef@ModuleDef(name, impl) if name.toString == "Gensym" =>
                      markExternPure(untpd.cpy.ModuleDef(moduleDef)(name, transformSub(impl)))

                    case defDef@DefDef(name, paramss, tpt, _) if name.toString == "eval" =>
                      subFunctions = subFunctions ++ (new SubFunGenerator(defDef)).getSubFuns
                      fakeFunctions = fakeFunctions :+ genFakeFun(defDef)
                      super.transform(defDef)

                    case other => super.transform(other)
                  } ++ fakeFunctions ++ subFunctions
                case _ => List(stat)
              })
            super.transform(cpy.PackageDef(tree)(pid, newStats))

          case _ => super.transform(tree)
        }
      case _ => tree
  }
}
