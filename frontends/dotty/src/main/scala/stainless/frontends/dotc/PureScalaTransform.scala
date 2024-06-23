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

/**
 * This class is a custom transformer for Scala code. It is similar to `MacroTransform`,
 * but it handles `untpdTree`, so it cannot directly inherit from `MacroTransform`.
 * It performs transformations on the Scala code to make it compatible with the Stainless verification tool.
 */
class PureScalaTransform extends Phase {

  import ast.untpd._

  // The name of this phase in the compiler pipeline
  override val phaseName = "pure Scala Transform"

  /**
   * The main method of this phase, which applies the transformation to the compilation unit.
   */
  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.untpdTree = atPhase(transformPhase)(newTransformer.transform(unit.untpdTree))
  }

  // Creates a new instance of the transformer
  private def newTransformer(using Context): PureScalaTransformer = new PureScalaTransformer

  // Returns the current phase
  private def transformPhase(using Context): Phase = this

  /**
   * This class performs the actual transformations on the Scala code.
   * It extends `UntypedTreeMap`, which is a class for transforming untyped trees.
   */
  private class PureScalaTransformer extends UntypedTreeMap {
    /**
     * The main method of this class, which performs the transformations on the given tree.
     * It matches on the structure of the tree and applies the appropriate transformation.
     */
    override def transform(tree: Tree)(using Context): Tree = {
      tree match {
        // Replace Int and Double with BigInt
        // Translating Double to Real might be a better choice, but it involves type conversion between BigInt and Real, which will be considered later.
        case Ident(name) if name.toString == "Int" || name.toString == "Double" =>
          name match {
            case name if name.isTermName =>
              Ident(termName("BigInt"))
            case name if name.isTypeName =>
              Ident(typeName("BigInt"))
          }
        // Despite there is implicit conversion between BigInt and Int,
        // there are still some cases where the conversion cannot be performed automatically (such as 1 -> "xxxx").
        // Therefore, all Numbers are directly wrapped with BigInt()
        case Number(_, _) =>
          Apply(Ident(termName("BigInt")), List(tree))
        // Replace Nil with Nil().
        case Ident(name) if name.toString == "Nil" =>
          Apply(Ident(termName("Nil")), Nil)
        // Replace None with None().
        case Ident(name) if name.toString == "None" =>
          Apply(Ident(termName("None")), Nil)
        // Replace A + B with plus(A, B).
        // Added plus functions in the stainless library that can handle addition of different types (int, string, ListMap).
        case InfixOp(left, op: Ident, right) if op.name == termName("+") =>
          right match
            case Tuple(tupleTrees: List[Tree]) =>
              Apply(Ident(termName("plus")), List(transform(left), Apply(Ident(termName("List")), tupleTrees.map(transform))))
            case _ =>
              Apply(Ident(termName("plus")), List(transform(left), transform(right)))
        // replace to with List.range
        case InfixOp(left, op: Ident, right) if op.name == termName("to") =>
          Apply(Select(Ident(termName("List")), termName("range")), List(transform(left), transform(right)))
        // Replace Character with String.
        // It is possible to add an implicit conversion from Char to String in the stainless library, but stainless cannot verify it because it must be @extern.
        case Literal(constant: Constants.Constant) if constant.value.isInstanceOf[Character] =>
          Literal(Constants.Constant(constant.value.toString))
        // Replace scala.collection.immutable.ListMap with ListMap.
        case Select(Select(Select(Ident(name1), name2), name3), name4) if s"$name1.$name2.$name3.$name4" == "scala.collection.immutable.ListMap" =>
          name4 match {
            case name if name.isTermName => Ident(termName("ListMap"))
            case name if name.isTypeName => Ident(typeName("ListMap"))
          }
        // Replace .abs with abs().
        case Select(qualifier, name) if name.toString == "abs" =>
          Apply(Ident(termName("abs")), List(qualifier))
        // Handling ListMap initialization.
        case Apply(fun, args) if (fun.isInstanceOf[Ident] && fun.asInstanceOf[Ident].name.toString == "ListMap"
          || fun.isInstanceOf[Select] && fun.asInstanceOf[Select].toString.endsWith("ListMap)")
          || fun.isInstanceOf[TypeApply] && fun.asInstanceOf[TypeApply].toString.contains("ListMap")) && args.nonEmpty =>
          args(0) match {
            // There is already a List wrapper.
            case Apply(fun2@Ident(name), args2) if name.toString == "List" =>
              Apply(transform(fun), transform(args))
            case _ =>
              // add List() wrapper for arguments of ListMap.
              // Adding direct support for initializing ListMap with multiple ArrowAssoc since stainless doesn't support SeqLiteral.
              Apply(transform(fun), List(Apply(Ident(termName("List")), transform(args))))
          }
        // replace sys.error() with error[Nothing]("Error message.")
        case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "sys" && name.toString == "error" =>
          Apply(TypeApply(Ident(termName("error")), List(Ident(typeName("Nothing")))), List(Literal(Constants.Constant("Error message."))))
        // replace math.xx with xx because the stainless.math library is imported.
        case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "math" =>
          Apply(Ident(name), transform(args))
        // ignore println
        case Apply(fun: Ident, args) if fun.name.toString == "println" =>
          EmptyTree
        // Replace throw with error[Nothing]("Error message.")
        case Throw(expr) =>
          // Although stainless supports the use of Exception(), its return type is not Nothing. Therefore, we use error[Nothing] instead.
          Apply(TypeApply(Ident(termName("error")), List(Ident(typeName("Nothing")))), List(Literal(Constants.Constant("Error message."))))
        // Add `import stainless.collection._` `import stainless.annotation._` `import stainless.lang._` and `import stainless.ext._` to the beginning of the file.
        case PackageDef(pid, stats) =>
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
          val importExt = Import(
            Select(Ident(termName("stainless")), termName("ext")),
            List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
          )
          cpy.PackageDef(tree)(transformSub(pid), importCollection :: importAnnotation :: importLang :: importMath :: importExt :: transformStats(stats, ctx.owner))
        // Remove all original imports.
        case Import(expr, selectors) =>
          EmptyTree
        case defDef@DefDef(name, paramss, tpt, _) =>
          val defDefDetector = new DefDefDetector(defDef)
          if (defDefDetector.unSupported) {
            val externIdent = Ident(typeName("extern"))
            // A very necessary step, otherwise errors will occur in the typer.
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
        case Match(selector, cases) =>
          // Find whether there is Alternative in cases
          val flatCases = cases.flatMap(case_ =>
            case_.pat match {
              case Alternative(elements) =>
                elements.collect { case element => CaseDef(element, case_.guard, case_.body) }
              case _ =>
                List(case_)
            })
          cpy.Match(tree)(transform(selector), transformSub(flatCases))
        case _ =>
          super.transform(tree)
      }
    }

    /**
     * This class is used to detect certain patterns in the definition of a function.
     * It traverses the tree of the function and collects information about it.
     */
    private class DefDefDetector(defDef: DefDef)(using Context) extends UntypedTreeTraverser {
      private val matches: Stack[Ident | Boolean] = Stack.empty
      private val cases: Stack[Ident] = Stack.empty
      val decreases: Set[Ident] = Set.empty
      var unSupported = false
      traverse(defDef)

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

      /**
       * This method traverses the tree and collects information about it.
       * It detects certain patterns in the tree and updates the state of the detector accordingly.
       */
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
          case Select(qualifier, name) if name.toString == "toString" || name.toString == "length" || name.toString == "isBlank" =>
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