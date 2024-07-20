package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}

class Assn1Preprocessor extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree = {
    tree match {
      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "sum" || name.toString == "power" =>
        val precondition = Apply(
          Ident(termName("require")),
          List(
            InfixOp(
              InfixOp(Ident(termName("n")), Ident(termName(">=")), Number("0", Whole(10))),
              Ident(termName("&&")),
              InfixOp(Ident(termName("n")), Ident(termName("<=")), Number("100", Whole(10))),
            )
          )
        )
        val newRhs = defDef.rhs match {
          case Block(stats, expr) =>
            Block(
              precondition :: stats,
              expr
            )
          case _ =>
            // The function body originally only had one statement. Wrap it in a block.
            Block(
              List(precondition),
              defDef.rhs
            )
        }
        cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs))
      case _ => super.transform(tree)
    }
  }
}
