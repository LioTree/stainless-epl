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
                  }
                case _ => List(stat)
              })
            super.transform(cpy.PackageDef(tree)(pid, newStats))

          case _ => super.transform(tree)
        }
      case _ => super.transform(tree)
  }
}
