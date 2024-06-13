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

  override def run(using dottyCtx: DottyContext): Unit = {
    val unit = dottyCtx.compilationUnit
    if(!unit.source.toString.startsWith("/tmp/stainless")) 
      (new PureScalaTransform).run
  }

  override def runOn(units: List[CompilationUnit])(using dottyCtx: DottyContext): List[CompilationUnit] = {
    super.runOn(units)
  }
}
