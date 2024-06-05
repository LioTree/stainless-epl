package stainless.frontends.dotc

import dotty.tools.dotc.ast.Trees.Untyped
import dotty.tools.dotc.ast.Trees.Tree

object ProgramExtractors {
  def processStats[T <: Untyped](stats: List[Tree[T]]): List[Tree[T]] = {
    println(stats)
    stats.toList
  }
}
