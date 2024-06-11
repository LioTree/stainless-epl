package stainless.frontends.dotc

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.ClassSymbol
import scala.collection.mutable.Set
import play.api.libs.json.Json

class TodoExtractor extends tpd.TreeTraverser {
  val functionNames = Set[String]()

  override def traverse(tree: tpd.Tree)(using ctx: Context): Unit = {
    tree match {
      case apply: tpd.Apply =>
        if (apply.fun.toString.equals("Select(Select(Ident(sys),package),error)") && apply.args(0).toString.contains("todo))")) {
          functionNames += ctx.owner.showFullName
        }
      case _ =>
    }
    traverseChildren(tree)
  }

  def writeJson(): Unit = {
    val json = Json.toJson(functionNames.toList)
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get("functionNames.json"), json.toString.getBytes(StandardCharsets.UTF_8))
  }
}