package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.equivchkplus.optExtractTarget

import scala.collection.mutable.{Map, Queue, Set, Stack}

class TargetExtractor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends ast.untpd.UntypedTreeMap {

  import ast.untpd.*

  private val depDetector = new DepDetector
  private val targets = depDetector.targets
  private val defStack: Stack[String] = Stack.empty

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
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

  class DepDetector(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends UntypedTreeTraverser {
    def insertElement(elements: Map[String, List[untpd.Tree]], key: String, element: untpd.Tree): Unit = {
      elements.updateWith(key) {
        case Some(existingList) => Some(element :: existingList)
        case None => Some(List(element))
      }
    }

    val packageDef = dottyCtx.compilationUnit.untpdTree match {
      case packageDef: PackageDef => packageDef
      case _ => throw new Exception("No package definition found")
    }
    val target = inoxCtx.options.findOption(optExtractTarget).getOrElse(throw new Exception("No target found"))
    val elements: Map[String, List[untpd.Tree]] = Map.empty

    packageDef.stats.foreach(stat =>
      stat match {
        case defDef: DefDef => insertElement(elements, defDef.name.toString, defDef)
        case typeDef: TypeDef => insertElement(elements, typeDef.name.toString, typeDef)
        case valDef: ValDef => insertElement(elements, valDef.name.toString, valDef)
        case moduleDef: ModuleDef => insertElement(elements, moduleDef.name.toString, moduleDef)
        case _ =>
      })
    
    val targets: Set[String] = Set(target)
    private val worklist: Queue[Tree] = Queue.empty
    worklist ++= elements(target)

    while (worklist.nonEmpty) {
      traverse(worklist.dequeue)
    }

    override def traverse(tree: untpd.Tree)(using DottyContext): Unit = {
      tree match {
        case Apply(Ident(name), args) if elements.contains(name.toString) && !targets.contains(name.toString) =>
          targets.add(name.toString)
          worklist ++= elements(name.toString)
          traverseChildren(tree)

        case Select(Ident(name), _) if elements.contains(name.toString) && !targets.contains(name.toString) =>
          targets.add(name.toString)
          worklist ++= elements(name.toString)
          traverseChildren(tree)

        case New(Ident(name)) if elements.contains(name.toString) && !targets.contains(name.toString) =>
          targets.add(name.toString)
          worklist ++= elements(name.toString)
          traverseChildren(tree)

        case ValDef(_, Ident(name), _) if elements.contains(name.toString) && !targets.contains(name.toString) =>
          targets.add(name.toString)
          worklist ++= elements(name.toString)
          traverseChildren(tree)

        case Template(_, parentsOrDerived: List[Tree], _, _) =>
          parentsOrDerived.foreach {
            case Ident(name) if elements.contains(name.toString) && !targets.contains(name.toString) =>
              targets.add(name.toString)
              worklist ++= elements(name.toString)
            case _ =>
          }
          traverseChildren(tree)

        case _ => traverseChildren(tree)
      }
    }
  }
}
