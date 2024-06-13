package stainless.frontends.dotc

import dotty.tools.dotc._
import core._
import Phases._
import ast.Trees._
import Contexts._

// Similar to `MacroTransform`, but we need to handle `untpdTree`,
// so we cannot directly inherit from it.
class PureScalaTransform extends Phase {
  import ast.untpd._

  override val phaseName = "pure Scala Transform"

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.untpdTree = atPhase(transformPhase)(newTransformer.transform(unit.untpdTree))
  }

  protected def newTransformer(using Context): PureScalaTransformer = new PureScalaTransformer

  protected def transformPhase(using Context): Phase = this

  class PureScalaTransformer extends UntypedTreeMap {
    override def transform(tree: Tree)(using Context): Tree = {
      tree match {
        case _ =>
          super.transform(tree)
      }
    }
  }
}
