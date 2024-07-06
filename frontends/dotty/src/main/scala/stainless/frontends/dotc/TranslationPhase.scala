package stainless.frontends.dotc

import dotty.tools.dotc.{Main as _, *}
import plugins.*
import core.Contexts.{Context as DottyContext, *}
import parsing.Parser
import typer.TyperPhase

class TranslationPhase extends PluginPhase {
  override val phaseName = "translation from Scala to Pure Scala"
  override val runsAfter = Set(Parser.name)
  override val runsBefore = Set(TyperPhase.name)
  var firstFile = true
  var publicPackageName = ""

  override def run(using dottyCtx: DottyContext): Unit = {
    val unit = dottyCtx.compilationUnit
    if (!unit.source.toString.startsWith("/tmp/stainless")) {
      println("Before PureScalaTransform: ")
      //      println(unit.untpdTree.show)
      //      println("-------------------------------------------------")
      //      println(unit.untpdTree.toString)
      val newPackageName = extractFileName(dottyCtx.compilationUnit.source.toString)
      if (firstFile) {
        (new PureScalaTransform(firstFile, newPackageName, newPackageName)).run
        publicPackageName = newPackageName
        firstFile = false
      }
      else
        (new PureScalaTransform(firstFile, publicPackageName, newPackageName)).run
      println("*************************************************")
      println(unit.untpdTree.show)
      println("-------------------------------------------------")
    }
    //      println(unit.untpdTree.toString)
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