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

  private def notTarget(name: String): Boolean = targets.nonEmpty && !targets.contains(name)

  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      // We only care about the top-level definitions
      case PackageDef(pid: Ident, stats) =>
        val newStats = stats.flatMap(
          stat => stat match {
            case TypeDef(name, _) if notTarget(name.toString) =>
              List(EmptyTree)
            case DefDef(name, _, _, _) if notTarget(name.toString) =>
              List(EmptyTree)
            case ModuleDef(name, _) if notTarget(name.toString) =>
              List(EmptyTree)
            case ValDef(name, _, _) if notTarget(name.toString) =>
              List(EmptyTree)
            case _ => List(stat)
          })
        cpy.PackageDef(tree)(pid, newStats)
      case _ => sys.error("Invalid Assignment")
    }
  }

  class DepDetector(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends UntypedTreeTraverser {
    private val packageDef = dottyCtx.compilationUnit.untpdTree match {
      case packageDef: PackageDef => packageDef
      case _ => throw new Exception("No package definition found")
    }
    private val elements: Map[String, List[untpd.Tree]] = Map.empty
    private val extendsMap: Map[String, List[String]] = Map.empty

    def insert[T](map: Map[String, List[T]])(key: String, value: T): Unit = {
      map.updateWith(key) {
        case Some(existingList) => Some(value :: existingList)
        case None => Some(List(value))
      }
    }

    packageDef.stats.foreach(stat =>
      stat match {
        case defDef: DefDef => insert(elements)(defDef.name.toString, defDef)

        case typeDef@TypeDef(name, rhs@Template(_, parentsOrDerived: List[Tree], _, _)) =>
          insert(elements)(name.toString, typeDef)
          parentsOrDerived.foreach {
            case Ident(parentName) => insert(extendsMap)(parentName.toString, name.toString)
            case _ =>
          }

        case typeDef@TypeDef(name, rhs: LambdaTypeTree) =>
          insert(elements)(name.toString, typeDef)

        case moduleDef@ModuleDef(name, impl@Template(_, parentsOrDerived: List[Tree], _, _)) =>
          insert(elements)(name.toString, moduleDef)
          parentsOrDerived.foreach {
            case Ident(parentName) => insert(extendsMap)(parentName.toString, name.toString)
            case _ =>
          }

        case valDef: ValDef => insert(elements)(valDef.name.toString, valDef)

        case _ =>
      })

    val targets: Set[String] = Set(inoxCtx.options.findOption(optExtractTarget).get: _*)
    private val worklist: Queue[Tree] = Queue.empty

    if (targets.nonEmpty) {
      worklist ++= targets.map(elements).flatten

      while (worklist.nonEmpty) {
        traverse(worklist.dequeue)
      }
    }

    def addWorklist(name: String): Unit = {
      if (elements.contains(name) && !targets.contains(name)) {
        targets.add(name)
        worklist ++= elements(name)
      }
    }

    override def traverse(tree: untpd.Tree)(using DottyContext): Unit = {
      tree match {
        case Apply(Ident(name), args) =>
          addWorklist(name.toString)
          traverseChildren(tree)

        case Select(Ident(name), _) =>
          addWorklist(name.toString)
          traverseChildren(tree)

        case New(Ident(name)) =>
          addWorklist(name.toString)
          traverseChildren(tree)

        case ValDef(_, Ident(name), _) =>
          addWorklist(name.toString)
          traverseChildren(tree)

        case ValDef(_, AppliedTypeTree(Ident(name), args), _) =>
          addWorklist(name.toString)
          args.foreach(arg =>
            arg match {
              case Ident(name) => addWorklist(name.toString)
              case _ =>
            })
          traverseChildren(tree)

        case TypeDef(name, _) =>
          if (extendsMap.contains(name.toString))
            extendsMap(name.toString).foreach(addWorklist)
          traverseChildren(tree)

        case ModuleDef(name, _) =>
          if (extendsMap.contains(name.toString))
            extendsMap(name.toString).foreach(addWorklist)
          traverseChildren(tree)

        case Template(_, parentsOrDerived: List[Tree], _, _) =>
          parentsOrDerived.foreach {
            case Ident(name) =>
              addWorklist(name.toString)
            case _ =>
          }
          traverseChildren(tree)

        case _ => traverseChildren(tree)
      }
    }
  }
}
