package stainless.frontends.dotc.purescala

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.{Ident, PackageDef}
import dotty.tools.dotc.core.Contexts.{Context as DottyContext, *}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.{Main as _, *}
import stainless.frontends.dotc.purescala.*

class TranslationPhase(val inoxCtx: inox.Context) extends PluginPhase {

  override val phaseName = "translation from Scala to Pure Scala"
  override val runsAfter = Set(Parser.name)
  override val runsBefore = Set(TyperPhase.name)
  var firstFile = true
  var publicPackageName = ""

  override def run(using dottyCtx: DottyContext): Unit = {
    val unit = dottyCtx.compilationUnit
    if (!unit.source.toString.startsWith("/tmp/stainless")) {
      println("Before Transformation: ")
      println(unit.untpdTree.show)
      println(unit.untpdTree.toString)

      given inox.Context = inoxCtx
      val transformers = (new ProgramExtractor).transform andThen
                         (new StainlessTransformer).transform andThen
                         (new DecreasesInference).transform andThen
                         (new PackageNameRewriter).transform

      unit.untpdTree = transformers(unit.untpdTree)

      println("*************************************************")
      println("After Transformation: ")
      println(unit.untpdTree.show)
      println(unit.untpdTree.toString)
    }
  }

  private def extractFileName(path: String): String = {
    val regex = """.*/([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName.replace("-", "_")
      case _ => "No match found"
    }
  }

  override def runOn(units: List[CompilationUnit])(using dottyCtx: DottyContext): List[CompilationUnit] = {
    super.runOn(units)
  }
}