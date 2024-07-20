package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.Spans.Span
import stainless.equivchkplus.{optExternPureDefs, optPublicClasses}
import stainless.frontends.dotc.epl.AssnProcessor.firstPackageName

class AssnProcessor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  private val pubClasses: Seq[String] = inoxCtx.options.findOption(optPublicClasses).getOrElse(Seq.empty[String])
  private val externpureDefs: Seq[String] = inoxCtx.options.findOption(optExternPureDefs).getOrElse(Seq.empty[String])

  private def extractFileName(path: String): String = {
    val regex = """(?:.*/)?([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName.replace("-", "_")
      case _ => sys.error("Invalid file name")
    }
  }

  private def genAnnotations(spanStart: Int): List[Apply] = {
    val externIdent = Ident(typeName("extern"))
    // A very necessary step, otherwise errors will occur in the typer.
    externIdent.span = Span(spanStart, spanStart + 7)
    val externAnnotation = Apply(Select(New(externIdent), termName("<init>")), Nil)

    val pureIdent = Ident(typeName("pure"))
    pureIdent.span = Span(spanStart + 8, spanStart + 8 + 5)
    val pureAnnotation = Apply(Select(New(pureIdent), termName("<init>")), Nil)
    List(externAnnotation, pureAnnotation)
  }

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      // Package Name Rewrite
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        val newPackageName = extractFileName(dottyCtx.source.toString)
        val newStats = {
          if (pubClasses.nonEmpty && AssnProcessor.firstPackageName != "") {
            val (tempNewStats, pubClassesNeed) = stats.map(stat =>
              stat match
                case DefDef(name, _, _, _) if pubClasses.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case ModuleDef(name, _) if pubClasses.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case TypeDef(name, _) if pubClasses.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case ValDef(name, _, _) if pubClasses.contains(name.toString) =>
                  (EmptyTree, name.toString)
                case _ => (stat, "")
            ).unzip
            val newImport = Import(Ident(termName(firstPackageName)),
              pubClassesNeed.filter(_ != "").map(
                publicClass => ImportSelector(Ident(termName(publicClass)))).toList
            )
            newImport :: tempNewStats
          }
          else {
            AssnProcessor.firstPackageName = newPackageName
            stats
          }
        }
        cpy.PackageDef(tree)(Ident(termName(newPackageName)), transformStats(newStats, dottyCtx.owner))

      // Add @extern and @pure
      case defDef@DefDef(name, paramss, tpt, _) if externpureDefs.contains(name.toString) =>
        val result = cpy.DefDef(tree)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs))
        result.withAnnotations(genAnnotations(result.span.start))

      case ModuleDef(name, impl) if externpureDefs.contains(name.toString) =>
        val result = untpd.cpy.ModuleDef(tree)(name, transformSub(impl))
        result.withAnnotations(genAnnotations(result.span.start))

      case valDef@ValDef(name, tpt, _) if externpureDefs.contains(name.toString) =>
        val result = cpy.ValDef(tree)(name, transform(tpt), transform(valDef.rhs))
        result.withAnnotations(genAnnotations(result.span.start))

      case _ => super.transform(tree)
    }
  }
}

object AssnProcessor {
  var firstPackageName = ""
}
