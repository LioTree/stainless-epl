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
    private val packageDef = dottyCtx.compilationUnit.untpdTree match {
      case packageDef: PackageDef => packageDef
      case _ => throw new Exception("No package definition found")
    }
    private val target = inoxCtx.options.findOption(optExtractTarget).getOrElse(throw new Exception("No target found"))
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

        case typeDef@TypeDef(name, rhs:LambdaTypeTree) =>
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

    val targets: Set[String] =Set(inoxCtx.options.findOption(optExtractTarget).getOrElse(throw new Exception("No extract target found")): _*)
    private val worklist: Queue[Tree] = Queue.empty
    worklist ++= targets.map(elements).flatten

    while (worklist.nonEmpty) {
      traverse(worklist.dequeue)
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
          if(extendsMap.contains(name.toString))
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
