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
                  impl.body
                case _ => List(stat)
              })
            super.transform(cpy.PackageDef(tree)(pid, newStats))

          case _ => sys.error("Invalid Assn2")
        }
      case _ => super.transform(tree)
  }
}
