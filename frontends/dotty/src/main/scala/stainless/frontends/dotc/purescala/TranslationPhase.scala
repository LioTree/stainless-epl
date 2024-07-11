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
      given inox.Context = inoxCtx
      val transformers = 
        (new ProgramExtractor).transform andThen
        (new StainlessTransformer).transform
        
      unit.untpdTree = transformers(unit.untpdTree)
      /*
      val packageName = extractFileName(unit.source.toString)
      // Replace original package name "<empty>" with the new package name
      val untypedTree = untpd.cpy.PackageDef(unit.untpdTree)(Ident(termName(packageName)), unit.untpdTree.asInstanceOf[PackageDef].stats)

      val extractTraverser = new ExtractTraverser("center", untypedTree)
      println(extractTraverser.targets)

      println("Before PureScalaTransform: ")
      println(untypedTree.show)
      println(untypedTree.toString)

      if (firstFile) {
        unit.untpdTree = new Assignment1Transformer(firstFile, packageName, extractTraverser.targets).transform(untypedTree)
        publicPackageName = packageName
        firstFile = false
      }
      else
        unit.untpdTree = new Assignment1Transformer(firstFile, publicPackageName, extractTraverser.targets).transform(untypedTree)
       */

      println("*************************************************")
      println(unit.untpdTree.show)
      println(unit.untpdTree.toString)
      println("-------------------------------------------------")
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