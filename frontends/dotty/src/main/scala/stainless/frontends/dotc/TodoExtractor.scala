package stainless.frontends.dotc

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.ClassSymbol
import scala.collection.mutable.Set
import play.api.libs.json.Json

object TodoExtractor extends tpd.TreeTraverser {
  val functions = Set[String]()

  override def traverse(tree: tpd.Tree)(using ctx: Context): Unit = {
    tree match {
      case apply: tpd.Apply =>
        if (apply.fun.toString.equals("Select(Select(Ident(sys),package),error)") && apply.args(0).toString.contains("todo))")) {
          println(ctx.owner.showFullName)
          functions += ctx.owner.showFullName
        }
      case _ =>
    }
    traverseChildren(tree)
  }
}