package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Names.{termName, typeName}

trait BaseTransformer(using DottyContext) extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  // `start` is used to perform some preprocessing work before calling `transform`.
  def start(tree: Tree)(using DottyContext): Tree = transform(tree)

  protected def errorWrapper = TypeApply(Ident(termName("errorWrapper")), List(Ident(typeName("Nothing"))))

  protected def typeIdent(name: String): Ident = Ident(typeName(name))

  protected def termIdent(name: String): Ident = Ident(termName(name))
}
