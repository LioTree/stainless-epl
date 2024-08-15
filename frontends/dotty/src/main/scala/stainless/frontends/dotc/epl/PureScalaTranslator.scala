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

import scala.collection.mutable.{ArrayBuffer, Map, Set, Stack}

/**
 * This class performs the transformations on the Scala code.
 * It extends `UntypedTreeMap`, which is a class for transforming untyped trees.
 */
class PureScalaTranslator(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends EPLTransformer {

  import ast.untpd.*

  /**
   * The main method of this class, which performs the transformations on the given tree.
   * It matches on the structure of the tree and applies the appropriate transformation.
   */
  override def transform(tree: Tree)(using dottyCtx: DottyContext): Tree = {
    tree match {
      /* Basic Type Int and String */
      // Replace Int,Integer with OverflowInt
      case Ident(name) if name.toString == "Int" || name.toString == "Integer" =>
        name match {
          case name if name.isTermName => termIdent("OverflowInt")
          case name if name.isTypeName => typeIdent("OverflowInt")
        }

      // Replace String with StringWrapper
      case Ident(name) if name.toString == "String" =>
        name match {
          case name if name.isTermName => termIdent("StringWrapper")
          case name if name.isTypeName => typeIdent("StringWrapper")
        }

      // Skip BigInt(n)
      case Apply(Ident(name), List(Number(digits, _))) if name == termName("BigInt") =>
        tree

      // all Numbers will be directly wrapped with OverflowInt(BigInt(n))
      // We don't use OverflowInt(n) because it has problem in unapply
      case Number(digits, _) =>
        buildOverflowIntLiteral(digits.toInt)

      // 'a' -> StringWrapper("a")
      // It is possible to add an implicit conversion from Char to String in the stainless library, but stainless cannot verify it because it must be @extern.
      case Literal(constant: Constants.Constant) if constant.value.isInstanceOf[Character] =>
        Apply(termIdent("StringWrapper"), List(Literal(Constants.Constant(constant.value.toString))))

      // "aaa" -> StringWrapper("aaa")
      case Literal(constant: Constants.Constant) if constant.value.isInstanceOf[String] =>
        Apply(termIdent("StringWrapper"), List(Literal(constant)))

      // a.toString() -> a.toStringWrapper
      case Apply(fun@Select(qualifier, name), args) if name.toString == "toString" && args.size == 0 =>
        Select(transform(qualifier), termName("toStringWrapper"))

      // a.toString -> a.toStringWrapper
      case Select(qualifier, name) if name.toString == "toString" =>
        Select(transform(qualifier), termName("toStringWrapper"))

      // a.toInt -> a.toOverflowInt
      case Select(qualifier, name) if name.toString == "toInt" =>
        Select(transform(qualifier), termName("toOverflowInt"))

      // a.length() -> a.length
      case Apply(fun@Select(qualifier, name), args) if name.toString == "length" && args.size == 0 =>
        Select(transform(qualifier), termName("length"))


      /* List, ListMap and Map */
      // Replace Nil with Nil().
      case Ident(name) if name.toString == "Nil" => Apply(termIdent("Nil"), Nil)

      // Replace None with None().
      case Ident(name) if name.toString == "None" => Apply(termIdent("None"), Nil)

      // map12 + (1 -> "a", 2 -> "b"); -> map12 + (1 -> "a") + (2 -> "b");
      case InfixOp(left, op: Ident, right: Tuple) if op.name == termName("+") => {
        def unrollPlusTuple(operands: List[Tree]): Tree = {
          operands match {
            case x :: Nil => transform(x)
            case x :: xs => InfixOp(unrollPlusTuple(xs), termIdent("+"), transform(x))
            case _ => sys.error("Unrolling plus tuple failed")
          }
        }

        val operands = (left :: right.trees).reverse
        unrollPlusTuple(operands)
      }

      // a::b => Cons(a,b)
      case InfixOp(left, op: Ident, right) if op.name == termName("::") =>
        Apply(termIdent("Cons"), List(transform(left), transform(right)))

      // Replace a until b with List.range(a,b)
      case InfixOp(left, op: Ident, right) if op.name == termName("until") =>
        Apply(buildSelect("List.range"), List(transform(left), transform(right)))

      // Replace a to b with List.rangeTo(a,b)
      case InfixOp(left, op: Ident, right) if op.name == termName("to") =>
        Apply(buildSelect("List.rangeTo"), List(transform(left), transform(right)))

      // Handling ListMap initialization.
      case Apply(fun, args) if (fun.isInstanceOf[Ident] && fun.asInstanceOf[Ident].name.toString == "ListMap"
        || fun.isInstanceOf[Select] && fun.asInstanceOf[Select].toString.endsWith("ListMap)")
        || fun.isInstanceOf[TypeApply] && fun.asInstanceOf[TypeApply].toString.contains("ListMap")) && args.nonEmpty =>
        args.size match {
          case 1 => {
            args(0) match
              // There is already a List wrapper.
              case Apply(fun2@Ident(name), args2) if name.toString == "List" =>
                Apply(transform(fun), transform(args))
              // There is a tuple. Like ListMap((41 -> "George H. W. Bush", 42 -> "Bill Clinton"))
              case tuple: Tuple if tuple.trees.size > 2 =>
                Apply(transform(fun), List(Apply(termIdent("List"), transform(tuple.trees))))
              case _ =>
                Apply(transform(fun), List(Apply(termIdent("List"), transform(args))))
          }
          case n =>
            // add List() wrapper for arguments of ListMap.
            // Adding direct support for initializing ListMap with multiple ArrowAssoc since stainless doesn't support SeqLiteral.
            Apply(transform(fun), List(Apply(termIdent("List"), transform(args))))
        }

      // import scala.collection.immutable.ListMap -> import stainless.collection.ListMap
      // scala.collection.immutable.ListMap.xx -> stainless.collection.ListMap.xx
      case Select(Select(Ident(name1), name2), name3) if s"$name1.$name2.$name3" == "scala.collection.immutable" =>
        buildSelect("stainless.collection")

      // import scala.collection.immutable.Map -> import stainless.lang.Map
      // scala.collection.immutable.Map.xx -> stainless.lang.Map.xx
      case Select(Select(Select(Ident(name1), name2), name3), name4) if s"$name1.$name2.$name3.$name4" == "scala.collection.immutable.Map" =>
        buildSelect("stainless.lang.Map")


      /* Exception */
      // replace sys.error() with errorWrapper[Nothing]
      case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "sys" && name.toString == "error" =>
        errorWrapper

      // Replace throw with error[Nothing]
      // Although stainless supports the use of Exception(), its return type is not Nothing. Therefore, we use error[Nothing] instead.
      case Throw(expr) =>
        errorWrapper

      case Match(selector, cases) =>
        // Find whether there is Alternative in cases
        // case a | b => c -> case a => c; case b => c
        val flatCases = cases.flatMap(case_ =>
          case_.pat match {
            case Alternative(elements) =>
              elements.collect { case element => CaseDef(element, case_.guard, case_.body) }
            case _ =>
              List(case_)
          })

        // Insert a `case _ => errorWrapper[Nothing]` in any case to pass match exhaustiveness verification.
        val defaultCase = CaseDef(termIdent("_"), EmptyTree, errorWrapper)
        cpy.Match(tree)(transform(selector), transformSub(flatCases :+ defaultCase))


      /* math */
      // import scala.math -> import stainless.math
      case Import(Ident(qualifierName), List(ImportSelector(Ident(name), EmptyTree, EmptyTree)))
        if qualifierName.toString == "scala" && name.toString == "math" =>
        buildImport("stainless.math")

      // import scala.math._ -> import stainless.math
      // scala.math.xx() -> stainless.math.xx()
      case Select(qualifier: Ident, name) if s"${qualifier.name}.$name" == "scala.math" =>
        buildSelect("stainless.math")


      /* Others */
      // Add `import stainless.collection._` `import stainless.annotation._` `import stainless.lang._` to the beginning of the file.
      case PackageDef(pid, stats) => {
        val importStainless = buildImport("stainless._")
        val importAnnotation = buildImport("stainless.annotation._")
        val importLang = buildImport("stainless.lang._")
        val importCollection = buildImport("stainless.collection._")

        cpy.PackageDef(tree)(transformSub(pid), importStainless :: importAnnotation ::
          importCollection :: importLang :: transformStats(stats, dottyCtx.owner))
      }

      // ignore println
      case Apply(fun: Ident, args) if fun.name.toString == "println" =>
        EmptyTree
      case _ =>
        super.transform(tree)
    }
  }
}