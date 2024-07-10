package stainless.frontends.dotc.purescala

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}

import scala.collection.mutable.{Queue, Map, Set, Stack}

class ProgramExtractor(target: String, packageDef: untpd.PackageDef)(using Context) extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  private val depDetector = new DepDetector
  private val targets = depDetector.targets
  private val defStack: Stack[String] = Stack.empty

  override def transform(tree: Tree)(using Context): Tree = {
    tree match {
      case TypeDef(name, rhs) =>
        if (defStack.isEmpty && !targets.contains(name.toString))
          TypeDef(name, Template(DefDef(termName("<init>"), Nil, TypeTree(), EmptyTree), Nil, EmptyValDef, Nil))
        else {
          defStack.push(name.toString)
          val result = cpy.TypeDef(tree)(name, transform(rhs))
          defStack.pop()
          result
        }

      case defDef@DefDef(name, paramss, tpt, _) =>
        if (defStack.isEmpty && !targets.contains(name.toString))
          DefDef(name, Nil, Ident(typeName("Unit")), Block(Nil, EmptyTree))
        else {
          defStack.push(name.toString)
          val result = cpy.DefDef(tree)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs))
          defStack.pop()
          result
        }

      case ModuleDef(name, impl) =>
        if (defStack.isEmpty && !targets.contains(name.toString))
          ModuleDef(name, Template(DefDef(termName("<init>"), Nil, TypeTree(), EmptyTree), Nil, EmptyValDef, Nil))
        else {
          defStack.push(name.toString)
          val result = untpd.cpy.ModuleDef(tree)(name, transformSub(impl))
          defStack.pop()
          result
        }

      case valDef@ValDef(name, tpt, _) =>
        if (defStack.isEmpty && !targets.contains(name.toString))
          ValDef(name, TypeTree(), Block(Nil, EmptyTree))
        else {
          defStack.push(name.toString)
          val result = cpy.ValDef(tree)(name, transform(tpt), transform(valDef.rhs))
          defStack.pop()
          result
        }

      case _ => super.transform(tree)
    }
  }

  class DepDetector(using Context) extends UntypedTreeTraverser {
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
}
