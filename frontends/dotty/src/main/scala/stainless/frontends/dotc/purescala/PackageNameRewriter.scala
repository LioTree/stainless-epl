package stainless.frontends.dotc.purescala

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.equivchkplus.{optPublicClasses, optPublicClassesPN}

class PackageNameRewriter(using inoxCtx: inox.Context) extends ast.untpd.UntypedTreeMap {
  import ast.untpd.*

  private val publicClasses = inoxCtx.options.findOption(optPublicClasses).getOrElse(Seq.empty[String])
  private val publicClassesPN = inoxCtx.options.findOption(optPublicClassesPN).getOrElse("")

  private def extractFileName(path: String): String = {
    val regex = """.*/([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName.replace("-", "_")
      case _ => "No match found"
    }
  }

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        val newPackageName = extractFileName(dottyCtx.source.toString)
        cpy.PackageDef(tree)(Ident(termName(newPackageName)), transformStats(stats, dottyCtx.owner))

      case Apply(Ident(name), args) if publicClasses.contains(name.toString) =>
        cpy.Apply(tree)(Select(Ident(termName(publicClassesPN)), name), transform(args))

      case Select(Ident(qualifierName), name) if publicClasses.contains(qualifierName.toString) =>
        cpy.Select(tree)(Select(Ident(termName(publicClassesPN)), qualifierName), name)

      case New(Ident(name)) if publicClasses.contains(name.toString) =>
        cpy.New(tree)(Select(Ident(termName(publicClassesPN)), name))

      case valDef@ValDef(name, Ident(tptName), _) if publicClasses.contains(tptName.toString) =>
        cpy.ValDef(valDef)(name, Select(Ident(termName(publicClassesPN)), tptName), transform(valDef.rhs))

      case _ => super.transform(tree)
    }
  }
}
