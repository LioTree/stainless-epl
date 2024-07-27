package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.{MemberDef as _, *}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.Spans.Span
import stainless.equivchkplus.{optExternPureDefs, optPublicDefs}

class EPLProcessor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends UntypedTransformer {

  import ast.untpd.*

  private val pubDefs: Seq[String] = inoxCtx.options.findOption(optPublicDefs).getOrElse(Seq.empty[String])
  private val externpures: Seq[String] = inoxCtx.options.findOption(optExternPureDefs).getOrElse(Seq.empty[String])

  private def extractFileName(path: String): String = {
    val regex = """(?:.*/)?([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName.replace("-", "_")
      case _ => sys.error("Invalid file name")
    }
  }

  protected def markExternPure(tree: MemberDef): MemberDef = {
    val spanStart = tree.span.start

    val externIdent = Ident(typeName("extern"))
    // A very necessary step, otherwise errors will occur in the typer.
    externIdent.span = Span(spanStart, spanStart + 7)
    val externAnnotation = Apply(Select(New(externIdent), termName("<init>")), Nil)

    val pureIdent = Ident(typeName("pure"))
    pureIdent.span = Span(spanStart + 8, spanStart + 8 + 5)
    val pureAnnotation = Apply(Select(New(pureIdent), termName("<init>")), Nil)

    tree.withAnnotations(List(externAnnotation, pureAnnotation))
  }

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      // Package Name Rewrite
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        val newPackageName = extractFileName(dottyCtx.source.toString)
        val newStats = {
          if (pubDefs.nonEmpty && EPLProcessor.firstPackageName != "") {
            val (tempNewStats, pubDefsNeed) = stats.map(stat =>
              stat match
                case DefDef(name, _, _, _) if pubDefs.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case ModuleDef(name, _) if pubDefs.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case TypeDef(name, _) if pubDefs.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case ValDef(name, _, _) if pubDefs.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case _ => (stat, "")
            ).unzip
            val pubDefsNeedSet: Set[String] = pubDefsNeed.filter(_ != "").toSet

            val newImport = Import(Ident(termName(EPLProcessor.firstPackageName)),
              pubDefsNeedSet.map(
                publicDef => ImportSelector(Ident(termName(publicDef)))).toList
            )
            newImport :: tempNewStats
          }
          else {
            EPLProcessor.firstPackageName = newPackageName
            stats
          }
        }
        cpy.PackageDef(tree)(Ident(termName(newPackageName)), transformStats(newStats, dottyCtx.owner))

      // Add @extern and @pure
      case defDef@DefDef(name, paramss, tpt, _) if externpures.contains(name.toString) =>
        markExternPure(cpy.DefDef(tree)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs)))

      case ModuleDef(name, impl) if externpures.contains(name.toString) =>
        markExternPure(untpd.cpy.ModuleDef(tree)(name, transformSub(impl)))

      case valDef@ValDef(name, tpt, _) if externpures.contains(name.toString) =>
        markExternPure(cpy.ValDef(tree)(name, transform(tpt), transform(valDef.rhs)))

      case _ => super.transform(tree)
    }
  }
}

object EPLProcessor {
  var firstPackageName = ""
}
