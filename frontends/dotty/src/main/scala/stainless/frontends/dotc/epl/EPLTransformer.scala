package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd.NumberKind.Whole
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.Spans.Span

trait EPLTransformer(using DottyContext) extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  // `start` is used to perform some preprocessing work before calling `transform`.
  def start(tree: Tree)(using DottyContext): Tree = transform(tree)

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

  protected def errorWrapper = TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing"))))

  protected def typeIdent(name: String): Ident = Ident(typeName(name))

  protected def termIdent(name: String): Ident = Ident(termName(name))

  protected def buildNumber(n: Int): Number = Number(n.toString, Whole(10))

  protected def buildSelect(input: String): Select | Ident = {
    def buildSelectTree(parts: List[String]): Select | Ident =
    parts match {
      case Nil => sys.error("Select tree cannot be empty")
      case head :: Nil => termIdent(head)
      case head :: tail => Select(buildSelectTree(tail), termName(head))
    }

    val parts = input.split("\\.")
    buildSelectTree(parts.toList.reverse)
  }

  protected def buildImport(input: String): Import = {
    val parts = input.split("\\.")
    val selector = ImportSelector(termIdent(parts.last), EmptyTree, EmptyTree)
    Import(buildSelect(parts.init.mkString(".")), List(selector))
  }
}
