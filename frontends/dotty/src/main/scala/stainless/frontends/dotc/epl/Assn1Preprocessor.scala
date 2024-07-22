package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.equivchkplus.optAssn1

class Assn1Preprocessor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends AssnProcessor {

  import ast.untpd.*

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree = {
    inoxCtx.options.findOption(optAssn1) match
      case Some(true) =>
        tree match {
          case defDef@DefDef(name, paramss, tpt, _) if name.toString == "sum" || name.toString == "power" || name.toString == "suffix" =>
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
            super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))

          case defDef@DefDef(name, paramss, tpt, _) if name.toString == "p" =>
            val precondition = Apply(
              Ident(termName("require")),
              List(
                InfixOp(
                  InfixOp(
                    InfixOp(Ident(termName("x")), Ident(termName(">=")), Number("0", Whole(10))),
                    Ident(termName("&&")),
                    InfixOp(Ident(termName("x")), Ident(termName("<=")), Number("100", Whole(10))),
                  ),
                  Ident(termName("&&")),
                  InfixOp(
                    InfixOp(Ident(termName("y")), Ident(termName(">=")), Number("0", Whole(10))),
                    Ident(termName("&&")),
                    InfixOp(Ident(termName("y")), Ident(termName("<=")), Number("100", Whole(10))),
                  ))
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
            super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))

          case _ => super.transform(tree)
        }
      case _ => super.transform(tree)
  }
}
