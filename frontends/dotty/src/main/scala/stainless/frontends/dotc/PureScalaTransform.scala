package stainless.frontends.dotc

import dotty.tools.dotc.*
import core.*
import Phases.*
import ast.Trees.*
import Contexts.*
import dotty.tools.dotc.ast.untpd
import Names.{EmptyTypeName, TermName, TypeName, termName, typeName}
import dotty.tools.dotc.ast.untpd.NumberKind.Whole
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

// Similar to `MacroTransform`, but we need to handle `untpdTree`,
// so we cannot directly inherit from it.
class PureScalaTransform extends Phase {

  import ast.untpd._

  override val phaseName = "pure Scala Transform"

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.untpdTree = atPhase(transformPhase)(newTransformer.transform(unit.untpdTree))
  }

  private def newTransformer(using Context): PureScalaTransformer = new PureScalaTransformer

  private def transformPhase(using Context): Phase = this

  private class PureScalaTransformer extends UntypedTreeMap {
    override def transform(tree: Tree)(using Context): Tree = {
      tree match {
        case Ident(name) if name.toString == "Double" =>
          name match {
            case name if name.isTermName =>
              // Replace Double with BigInt.
              Ident(termName("BigInt"))
            case name if name.isTypeName =>
              Ident(typeName("BigInt"))
          }
        case Ident(name) if name.toString == "Int" =>
          // Replace Int with BigInt.
          name match {
            case name if name.isTermName =>
              Ident(termName("BigInt"))
            case name if name.isTypeName =>
              Ident(typeName("BigInt"))
          }
        case Ident(name) if name.toString == "Nil" =>
          // Replace Nil with Nil().
          Apply(Ident(termName("Nil")), Nil)
        case Ident(name) if name.toString == "None" =>
          // Replace None with None().
          Apply(Ident(termName("None")), Nil)
        case Select(Select(Select(Ident(name1), name2), name3), name4) if s"$name1.$name2.$name3.$name4" == "scala.collection.immutable.ListMap" =>
          // Replace scala.collection.immutable.ListMap with ListMap.
          name4 match {
            case name if name.isTermName => Ident(termName("ListMap"))
            case name if name.isTypeName => Ident(typeName("ListMap"))
          }
        case Apply(fun, args) if (fun.isInstanceOf[Ident] && fun.asInstanceOf[Ident].name.toString == "ListMap"
          || fun.isInstanceOf[Select] && fun.asInstanceOf[Select].toString.endsWith("ListMap)")) && args.nonEmpty =>
          args(0) match {
            // There is already a List wrapper.
            case Apply(fun2@Ident(name), args2) if name.toString == "List" =>
              Apply(transform(fun), transform(args))
            case _ =>
              // add List() wrapper for arguments of ListMap.
              // Adding direct support for initializing ListMap with multiple ArrowAssoc in the stainless library seems to cause a bug in stainless codeExtraction (lack of handling for SeqLiteral).
              Apply(transform(fun), List(Apply(Ident(termName("List")), transform(args))))
          }
        case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "sys" && name.toString == "error" =>
          // replace sys.error() with error[Nothing]("Error message.")
          Apply(TypeApply(Ident(termName("error")), List(Ident(typeName("Nothing")))), List(Literal(Constants.Constant("Error message."))))
        case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "math" =>
          // replace math.xx with xx because the stainless.math library is imported.
          Apply(Ident(name), args)
        case Apply(fun:Ident, args) if fun.name.toString == "println" =>
          // replace println with print
          Apply(Ident(termName("print")), args)
        case Throw(expr) =>
          // Replace throw with error[Nothing]("Error message.")
          // Although stainless supports the use of Exception(), its return type is not Nothing.
          
//          Apply(Ident(termName("Exception")), List(Literal(Constants.Constant("Exception message."))))
          Apply(TypeApply(Ident(termName("error")), List(Ident(typeName("Nothing")))), List(Literal(Constants.Constant("Error message."))))
        case Number(_, _) =>
           // Despite there is implicit conversion between BigInt and Int,
           // there are still some cases where the conversion cannot be performed automatically (such as 1 -> "xxxx").
           // Therefore, all Numbers are directly wrapped with BigInt()
          Apply(Ident(termName("BigInt")), List(tree))
        case PackageDef(pid, stats) =>
          // Add `import stainless.collection._` `import stainless.annotation._` and
          // `import stainless.lang._` to the beginning of the file.
          val importCollection = Import(
            Select(Ident(termName("stainless")), termName("collection")),
            List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
          )
          val importAnnotation = Import(
            Select(Ident(termName("stainless")), termName("annotation")),
            List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
          )
          val importLang = Import(
            Select(Ident(termName("stainless")), termName("lang")),
            List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
          )
          val importMath = Import(
            Select(Ident(termName("stainless")), termName("math")),
            List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
          )
          cpy.PackageDef(tree)(transformSub(pid), importCollection :: importAnnotation :: importLang :: importMath :: transformStats(stats, ctx.owner))
        case Import(expr, selectors) =>
          // Remove all imports.
          EmptyTree
        case defDef@DefDef(name, paramss, tpt, _) =>
            val defDefDetector = new DefDefDetector(defDef)
            defDefDetector.traverse(defDef)
            if(defDefDetector.unSupported || name.toString == "nameFromNum") {
              val externIdent = Ident(typeName("extern"))
              // Translation to English: A very necessary step, otherwise errors will occur in the typer.
              // It took two out of three days to find the problem...
              externIdent.span = Span(defDef.span.start, defDef.span.start + 7)
              val externAnnotation = Apply(Select(New(externIdent), termName("<init>")), Nil)

              val pureIdent = Ident(typeName("pure"))
              pureIdent.span = Span(defDef.span.start + 8, defDef.span.start + 8 + 5)
              val pureAnnotation = Apply(Select(New(pureIdent), termName("<init>")), Nil)

              cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs)).withAnnotations(List(externAnnotation, pureAnnotation))
            }
            else {
              // Add decreases annotation automatically.
              if (!defDefDetector.decreases.isEmpty) {
                val decreasesApplies: ArrayBuffer[Apply] = ArrayBuffer.empty
                defDefDetector.decreases.foreach(decrease =>
                  decreasesApplies += Apply(Ident(termName("decreases")), List(decrease))
                )
                val newRhs = defDef.rhs match {
                  case Block(stats, expr) =>
                    Block(
                      decreasesApplies.toList ::: stats,
                      expr
                    )
                  case _ =>
                    // The function body originally only had one statement. Wrap it in a block.
                    Block(
                      decreasesApplies.toList,
                      defDef.rhs
                    )
                }
                // remove all original annotations
                cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)).withAnnotations(Nil)
              }
              else
                // remove all original annotations
                cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs)).withAnnotations(Nil)
            }
        case _ =>
          super.transform(tree)
      }
    }

    private class DefDefDetector(defDef: DefDef) extends UntypedTreeTraverser {
      private val matches: Stack[Ident | Boolean] = Stack.empty
      private val cases: Stack[Ident] = Stack.empty
      val decreases: Set[Ident] = Set.empty
      var unSupported = false

      private def checkParamss(target: Ident): Boolean = {
        defDef.paramss.exists { params =>
          params.exists { param =>
            param match {
              case ValDef(name, tpt, _) if name == target.name =>
                tpt match {
                  case AppliedTypeTree(Ident(tptName), args) if tptName.toString == "List" =>
                    true
                  case _ =>
                    false
                }
              case _ =>
                false
            }
          }
        }
      }

      override def traverse(tree: untpd.Tree)(using Context): Unit = {
        tree match {
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
              case identSelector@Ident(name) if checkParamss(identSelector) =>
                matches.push(identSelector)
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
          case Apply(fun: Select, args) if fun.toString.endsWith("toString)") || fun.toString.endsWith("length)") =>
            unSupported = true
          case Select(qualifier, name) if name.toString == "toString" || name.toString == "length" =>
            unSupported = true
          case ForDo(enums, body) =>
            unSupported = true
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
}
