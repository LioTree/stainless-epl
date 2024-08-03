package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Names.{EmptyTermName, EmptyTypeName, TermName, TypeName, termName, typeName}
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable.{ArrayBuffer, Set, Stack, Map}

class DecreasesInference(using dottyCtx: DottyContext) extends BaseTransformer {

  import ast.untpd.*

  override def transform(tree: Tree)(using DottyContext): Tree = {
    tree match {
      case defDef@DefDef(name, paramss, tpt, _) =>
        val decreaseDetector = new DecreaseDetector(defDef)
        if (!decreaseDetector.decreases.isEmpty) {
          val decreasesApplies: ArrayBuffer[Apply] = ArrayBuffer.empty
          decreaseDetector.decreases.foreach(decrease =>
            decreasesApplies += Apply(Ident(termName("decreases")), List(decrease))
            )
          val newRhs = defDef.rhs match {
            case Block(stats, expr) => Block(decreasesApplies.toList ::: stats, expr)
            // The function body originally only had one statement. Wrap it in a block.
            case _ => Block(decreasesApplies.toList, defDef.rhs)
          }
          cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs))
        }
        else
          cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs))

      case _ => super.transform(tree)
    }
  }

  /**
   * This class is used to detect certain patterns which need decreases in the definition of a function.
   * It traverses the tree of the function and collects information about it.
   */
  private class DecreaseDetector(defDef: DefDef)(using dottyCtx: DottyContext) extends UntypedTreeTraverser {
    private val matches: Stack[Ident | Boolean] = Stack.empty
    private val cases: Stack[Ident] = Stack.empty
    private val listParamss: Set[String] = Set.empty
    private val var2Param: Map[String, String] = Map.empty
    val decreases: Set[Ident] = Set.empty
    initListParamss()
    traverse(defDef)

    private def initListParamss(): Unit = {
      defDef.paramss.foreach { params =>
        params.foreach { param =>
          param match
            case ValDef(name, tpt, _) =>
              tpt match {
                case AppliedTypeTree(Ident(tptName), args) if tptName.toString == "List" =>
                  listParamss.add(name.toString)
                  var2Param += (name.toString -> name.toString)
                case _ =>
              }
            case _ =>
        }
      }
    }

    /**
     * This method traverses the tree and collects information about it.
     * It detects certain patterns in the tree and updates the state of the detector accordingly.
     */
    override def traverse(tree: untpd.Tree)(using dottyCtx: DottyContext): Unit = {
      tree match {
        // cases like val r = reverse(l) r match {}
        case ValDef(name, _, Apply(fun:Ident, args)) if fun.name != defDef.name =>
          args.foreach(arg =>
            arg match {
              case Ident(name2) if listParamss.contains(name2.toString) =>
                listParamss.add(name.toString)
                var2Param += (name.toString -> name2.toString)
              case _ =>
            }
          )
        case CaseDef(pat, guard, body) =>
          pat match {
            case InfixOp(_, op: Ident, right: Ident) if op.name == termName("::") && matches.top != false =>
              cases.push(right)
              traverseChildren(tree)
              cases.pop()
            case _ =>
              traverseChildren(tree)
          }
        case Match(selector, cases) =>
          selector match {
            case identSelector@Ident(name) if listParamss.contains(name.toString) =>
              matches.push(Ident(termName(var2Param(name.toString))))
              traverseChildren(tree)
              matches.pop()
            case _ =>
              matches.push(false)
              traverseChildren(tree)
              matches.pop()
          }
        case Apply(fun@Ident(name), args) if fun.name == defDef.name =>
          args.foreach(arg =>
            arg match {
              case argIdent: Ident if cases.exists(caseIdent => caseIdent.name == argIdent.name) =>
                findIdentInMatches match {
                  case Some(top) =>
                    decreases += top
                  case None =>
                }
              case _ =>
            }
          )
          traverseChildren(tree)
        case _ =>
          traverseChildren(tree)
      }
    }

    def findIdentInMatches: Option[Ident] = {
      matches.collectFirst {
        case ident: Ident => ident
      }
    }
  }
}
