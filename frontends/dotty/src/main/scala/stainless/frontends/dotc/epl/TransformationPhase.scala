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
import stainless.equivchkplus.{optTransformation, optAssn2}

class TransformationPhase(val inoxCtx: inox.Context) extends PluginPhase {

  override val phaseName = "Transformation for Programming assignments of EPL"
  override val runsAfter = Set(Parser.name)
  override val runsBefore = Set(TyperPhase.name)
  var firstFile = true
  var publicPackageName = ""

  override def run(using dottyCtx: DottyContext): Unit = {
    inoxCtx.options.findOption(optTransformation) match {
      case Some(to) if to == true =>
        val unit = dottyCtx.compilationUnit
        if (!unit.source.toString.startsWith("/tmp/stainless")) {
          println("Before Transformation: ")
          println(unit.untpdTree.show)
          println(unit.untpdTree.toString)

          if(inoxCtx.options.findOption(optAssn2).getOrElse(false))
            unit.untpdTree = (new Assn2Preprocessor).transform(unit.untpdTree)

          given inox.Context = inoxCtx

          val transformers = (new TargetExtractor).transform andThen
            (new PureScalaTranslator).transform andThen
            (new DecreasesInference).transform andThen
            (new PostProcessor).transform

          unit.untpdTree = transformers(unit.untpdTree)

          println("*************************************************")
          println("After Transformation: ")
          println(unit.untpdTree.show)
          println(unit.untpdTree.toString)
        }
      case _ =>
    }
  }

  override def runOn(units: List[CompilationUnit])(using dottyCtx: DottyContext): List[CompilationUnit] = {
    super.runOn(units)
  }
}