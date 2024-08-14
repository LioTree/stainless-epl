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
      // Replace Int,Integer and Double with OverflowInt
      case Ident(name) if name.toString == "Int" || name.toString == "Integer" =>
        name match {
          case name if name.isTermName => termIdent("OverflowInt")
          case name if name.isTypeName => typeIdent("OverflowInt")
        }

      case Ident(name) if name.toString == "String" =>
        name match {
          case name if name.isTermName => termIdent("StringWrapper")
          case name if name.isTypeName => typeIdent("StringWrapper")
        }

      // Skip BigInt(n)
      case Apply(Ident(name), List(Number(digits, _))) if name.toString == "BigInt" =>
        tree

      // all Numbers are directly wrapped with OverflowInt(BigInt(n))
      // We don't use OverflowInt(n) because it has problem in unapply
      case Number(digits, _) =>
        Apply(termIdent("OverflowInt"), List(Apply(termIdent("BigInt"), List(tree))))

      // Replace Nil with Nil().
      case Ident(name) if name.toString == "Nil" => Apply(termIdent("Nil"), Nil)

      // Replace None with None().
      case Ident(name) if name.toString == "None" => Apply(termIdent("None"), Nil)

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
      case InfixOp(left, op:Ident, right) if op.name == termName("::") =>
        Apply(termIdent("Cons"), List(transform(left), transform(right)))

      // Replace a until b with List.range(a,b)
      case InfixOp(left, op: Ident, right) if op.name == termName("until") =>
        Apply(buildSelect("List.range"), List(transform(left), transform(right)))

      // Replace a to b with List.rangeTo(a,b)
      case InfixOp(left, op: Ident, right) if op.name == termName("to") =>
        Apply(buildSelect("List.rangeTo"), List(transform(left), transform(right)))

      // In Scala, for comprehensions are merely syntactic sugar that get translated into calls to methods like foreach, map, and flatMap.
      // Stainless can support List's for-yield but cannot support for, because List lacks the foreach method.
      // Adding foreach is certainly simple, but Stainless cannot handle lambda functions with side effects.
      // This means scenarios like var result = 0; for (i <- list) { result += i } cannot be supported even List::foreach is added.
      // Therefore, we convert for loops directly into while loops for processing, and Stainless will transform them into recursive functions.
      //      case ForDo(List(GenFrom(pat, expr, checkMode)), body) =>
      //        val counterVarName: String = randomVariableName(8)
      //        val exprVarName: String = randomVariableName(8)
      //        val exprDef = ValDef(termName(exprVarName), TypeTree(), transform(expr))
      //        val counterDef = ValDef(termName(counterVarName), TypeTree(), Apply(termIdent("BigInt"), List(buildNumber(0))))
      //        counterDef.setMods(Modifiers(Flags.Mutable))
      //        val whileDo = WhileDo(
      //          Parens(InfixOp(termIdent(counterVarName), termIdent("<"), Select(termIdent(exprVarName), termName("length")))),
      //          Block(
      //            List(
      //              Apply(termIdent("decreases"), List(InfixOp(Select(termIdent(exprVarName), termName("length")),
      //                termIdent("-"), termIdent(counterVarName)))),
      //              ValDef(pat.asInstanceOf[Ident].name.toTermName, TypeTree(), Apply(termIdent(exprVarName),
      //                List(termIdent(counterVarName)))),
      //              transform(body),
      //            ),
      //            Assign(termIdent(counterVarName), InfixOp(termIdent(counterVarName), termIdent("+"),
      //              Apply(termIdent("OverflowInt"), List(buildNumber(1)))))
      //          )
      //        )
      //        Block(List(exprDef, counterDef), InfixOp(Parens(whileDo), termIdent("invariant"),
      //          Parens(InfixOp(termIdent(counterVarName), termIdent(">="),
      //            Apply(termIdent("BigInt"), List(buildNumber(0)))))))

      // Replace Character with String.
      // It is possible to add an implicit conversion from Char to String in the stainless library, but stainless cannot verify it because it must be @extern.
      case Literal(constant: Constants.Constant) if constant.value.isInstanceOf[Character] =>
        Apply(termIdent("StringWrapper"), List(Literal(Constants.Constant(constant.value.toString))))

      case Literal(constant: Constants.Constant) if constant.value.isInstanceOf[String] =>
        Apply(termIdent("StringWrapper"), List(Literal(constant)))

      case Apply(fun@Select(qualifier, name), args) if name.toString == "toString" && args.size == 0 =>
        Select(transform(qualifier), termName("toStringWrapper"))

      case Select(qualifier, name) if name.toString == "toString" =>
        Select(transform(qualifier), termName("toStringWrapper"))

      case Select(qualifier, name) if name.toString == "toInt" =>
        Select(transform(qualifier), termName("toOverflowInt"))

      case Apply(fun@Select(qualifier, name), args) if name.toString == "length" && args.size == 0 =>
        Select(transform(qualifier), termName("length"))

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

      // replace sys.error() with errorWrapper[Nothing]
      case Apply(fun@Select(qualifier: Ident, name: TermName), args) if qualifier.name.toString == "sys" && name.toString == "error" =>
        errorWrapper

      // ignore println
      case Apply(fun: Ident, args) if fun.name.toString == "println" =>
        EmptyTree

      // Replace throw with error[Nothing]("Error message.")
      case Throw(expr) =>
        // Although stainless supports the use of Exception(), its return type is not Nothing. Therefore, we use error[Nothing] instead.
        errorWrapper

      // Just make Stainless happy. It will throw an error if non-sealed classes are compared.
      case typeDef@TypeDef(name, rhs) if typeDef.mods is Flags.Abstract =>
        val result = cpy.TypeDef(tree)(name, transform(rhs))
        result.withMods(result.mods | Flags.Sealed)

      case defDef@DefDef(name, paramss, tpt, _) =>
        cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(defDef.rhs))

      // Add `import stainless.collection._` `import stainless.annotation._` `import stainless.lang._` to the beginning of the file.
      case PackageDef(pid, stats) => {
        val importStainless = buildImport("stainless._")
        val importAnnotation = buildImport("stainless.annotation._")
        val importLang = buildImport("stainless.lang._")
        val importCollection = buildImport("stainless.collection._")

        cpy.PackageDef(tree)(transformSub(pid), importStainless :: importAnnotation ::
          importCollection :: importLang :: transformStats(stats, dottyCtx.owner))
      }

      // import scala.math => import stainless.math
      case Import(Ident(qualifierName), List(ImportSelector(Ident(name), EmptyTree, EmptyTree)))
        if qualifierName.toString == "scala" && name.toString == "math" =>
        buildImport("stainless.math")

      // import scala.math._ => import stainless.math
      // scala.math.xx() => stainless.math.xx()
      case Select(qualifier: Ident, name) if s"${qualifier.name}.$name" == "scala.math" =>
        buildSelect("stainless.math")

      // import scala.collection.immutable.ListMap => import stainless.collection.ListMap
      // scala.collection.immutable.ListMap.xx => stainless.collection.ListMap.xx
      case Select(Select(Ident(name1), name2), name3) if s"$name1.$name2.$name3" == "scala.collection.immutable" =>
        buildSelect("stainless.collection")

      // import scala.collection.immutable.Map => import stainless.lang.Map
      // scala.collection.immutable.Map.xx => stainless.lang.Map.xx
      case Select(Select(Select(Ident(name1), name2), name3), name4) if s"$name1.$name2.$name3.$name4" == "scala.collection.immutable.Map" =>
        buildSelect("stainless.lang.Map")

      case Match(selector, cases) =>
        // Find whether there is Alternative in cases
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

      case _ =>
        super.transform(tree)
    }
  }

  private def randomVariableName(length: Int): String = {
    import scala.util.Random
    require(length > 0, "Variable name length must be greater than 0")

    val alphabet = ('a' to 'z') ++ ('A' to 'Z')
    val validChars = alphabet ++ ('0' to '9') ++ Seq('_')

    val firstChar = alphabet(Random.nextInt(alphabet.length))
    val remainingChars = (1 until length).map(_ => validChars(Random.nextInt(validChars.length)))
    (firstChar +: remainingChars).mkString
  }
}