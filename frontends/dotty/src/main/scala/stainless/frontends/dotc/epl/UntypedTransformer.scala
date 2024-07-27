package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext

trait UntypedTransformer extends ast.untpd.UntypedTreeMap {
  import ast.untpd.*

  // `start` is used to perform some preprocessing work before calling `transform`.
  def start(tree: Tree)(using DottyContext): Tree = transform(tree)
}
