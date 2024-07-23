package stainless.frontends.dotc.epl

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.{Ident, PackageDef}
import dotty.tools.dotc.core.Contexts.{Context as DottyContext, *}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.{Main as _, *}
import stainless.frontends.dotc.epl.*
import stainless.equivchkplus.{DebugSectionTransformation, optTransformation, optAssn2}

class TransformationPhase(val inoxCtx: inox.Context) extends PluginPhase {

  override val phaseName = "Transformation for Programming assignments of EPL"
  override val runsAfter = Set(Parser.name)
  override val runsBefore = Set(TyperPhase.name)
  var firstFile = true
  var publicPackageName = ""

  override def run(using dottyCtx: DottyContext): Unit = {
    inoxCtx.options.findOption(optTransformation) match {
      case Some(true)  =>
        val unit = dottyCtx.compilationUnit
        if (!unit.source.toString.startsWith("/tmp/stainless")) {
          given inox.Context = inoxCtx
          given givenDebugSection: DebugSectionTransformation.type = DebugSectionTransformation

          inoxCtx.reporter.whenDebug(DebugSectionTransformation) { debug =>
            debug(s"Before transformation:\n ${unit.untpdTree.show}")
            debug(s"${unit.untpdTree.toString}")
          }

          val transformers = (new TargetExtractor).transform andThen
            (new Assn1Preprocessor).transform andThen
            (new Assn2Preprocessor).transform andThen
            (new DecreasesInference).transform andThen
            (new PureScalaTranslator).transform

          unit.untpdTree = transformers(unit.untpdTree)

          inoxCtx.reporter.whenDebug(DebugSectionTransformation) { debug =>
            debug(s"After transformation:\n ${unit.untpdTree.show}")
            debug(s"${unit.untpdTree.toString}")
          }
        }
      case _ =>
    }
  }

  override def runOn(units: List[CompilationUnit])(using dottyCtx: DottyContext): List[CompilationUnit] = {
    super.runOn(units)
  }
}