package stainless.frontends.dotc

import dotty.tools.dotc.{Main as _, *}
import plugins.*
import core.Contexts.{Context as DottyContext, *}
import parsing.Parser
import stainless.frontends.dotc.purescala.{Assignment1Transformer, PureScalaTransformer}
import typer.TyperPhase
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.{PackageDef, Ident}
import dotty.tools.dotc.core.Names.termName

class TranslationPhase extends PluginPhase {

  override val phaseName = "translation from Scala to Pure Scala"
  override val runsAfter = Set(Parser.name)
  override val runsBefore = Set(TyperPhase.name)
  var firstFile = true
  var publicPackageName = ""

  override def run(using dottyCtx: DottyContext): Unit = {

    val unit = dottyCtx.compilationUnit
    if (!unit.source.toString.startsWith("/tmp/stainless")) {
      val packageName = extractFileName(unit.source.toString)
      // Replace original package name "<empty>" with the new package name
      val untypedTree = untpd.cpy.PackageDef(unit.untpdTree)(Ident(termName(packageName)), unit.untpdTree.asInstanceOf[PackageDef].stats)

      println("Before PureScalaTransform: ")
      println(untypedTree.show)
      println(untypedTree.toString)

      if (firstFile) {
        unit.untpdTree = new Assignment1Transformer(firstFile, packageName).transform(untypedTree)
        publicPackageName = packageName
        firstFile = false
      }
      else
        unit.untpdTree = new Assignment1Transformer(firstFile, publicPackageName).transform(untypedTree)

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