package stainless.frontends.dotc.purescala

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.ast.Trees.Ident
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class ExtractTraverser(target: String, packageDef: PackageDef)(using Context) extends UntypedTreeTraverser {
  val elements: Map[String, untpd.Tree] = Map.empty

  packageDef.stats.foreach(stat =>
    stat match {
      case defDef: DefDef =>
        elements += (defDef.name.toString -> defDef)
      case typeDef: TypeDef =>
        elements += (typeDef.name.toString -> typeDef)
      case valDef: ValDef =>
        elements += (valDef.name.toString -> valDef)
      case moduleDef: ModuleDef =>
        elements += (moduleDef.name.toString -> moduleDef)
      case _ =>
    }
  )
  val targets: Set[String] = Set(target)
  private val worklist: Queue[Tree] = Queue(elements(target))

  while (worklist.nonEmpty) {
    traverse(worklist.dequeue)
  }

  override def traverse(tree: untpd.Tree)(using Context): Unit = {
    tree match {
      case Apply(Ident(name), args) if elements.contains(name.toString) && !targets.contains(name.toString) =>
        targets.add(name.toString)
        worklist.enqueue(elements(name.toString))
        traverseChildren(tree)

      case Select(Ident(name), _) if elements.contains(name.toString) && !targets.contains(name.toString) =>
        targets.add(name.toString)
        worklist.enqueue(elements(name.toString))
        traverseChildren(tree)

      case New(Ident(name)) if elements.contains(name.toString) && !targets.contains(name.toString) =>
        targets.add(name.toString)
        worklist.enqueue(elements(name.toString))
        traverseChildren(tree)

      case ValDef(_, Ident(name), _) if elements.contains(name.toString) && !targets.contains(name.toString) =>
        targets.add(name.toString)
        worklist.enqueue(elements(name.toString))
        traverseChildren(tree)

      case Template(_, parentsOrDerived: List[Tree], _, _) =>
        parentsOrDerived.foreach {
          case Ident(name) if elements.contains(name.toString) && !targets.contains(name.toString) =>
            targets.add(name.toString)
            worklist.enqueue(elements(name.toString))
          case _ =>
        }

      case _ => traverseChildren(tree)
    }
  }
}
