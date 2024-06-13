package stainless.frontends.dotc

import dotty.tools.dotc.{Main as _, *}
import plugins.*
import core.Contexts.{Context as DottyContext, *}

class TranslationPhase extends PluginPhase {
  override val name: String = "translation from Scala to Pure Scala"
  override val runsAfter = ???
  override val runsBefore = ???

  override def run(using dottyCtx: DottyContext): Unit = {
    ???
  }

  override def runOn(units: List[CompilationUnit])(using dottyCtx: DottyContext): List[CompilationUnit] = {
    super.runOn(units)
  }
}
