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

class PackageNameRewriter(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends EPLTransformer {

  import ast.untpd.*

  protected def extractFileName(path: String): String = {
    val regex = """(?:.*/)?([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName
      case _ => sys.error("Invalid file name")
    }
  }

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      // Package Name Rewrite
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        val newPackageName = extractFileName(dottyCtx.source.toString)
        cpy.PackageDef(tree)(termIdent(newPackageName), transformStats(stats, dottyCtx.owner))

      case _ => super.transform(tree)
    }
  }
}
