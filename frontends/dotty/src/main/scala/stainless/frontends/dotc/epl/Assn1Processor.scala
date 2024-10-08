package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}

class Assn1Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context)
  extends PureScalaTranslator
    with TransformContext {

  import ast.untpd.*

  private val framework = Set(
    "power",
    "Colour",
    "Red",
    "Green",
    "Blue",
    "Shape",
    "Circle",
    "Rectangle",
  )

  private val translateDouble: Boolean = targets.contains("boundingBox") || targets.contains("mayOverlap")
  private val useMap: Boolean = targets.contains("list2map") || targets.contains("election")

  override def start(tree: untpd.Tree)(using DottyContext): untpd.Tree = {
    if (assn1)
      transform(tree)
    else if (!assn2)
      super.transform(tree)
    else
      tree
  }

  private def getPrecondition(n: String): Apply = {
    val overflowInt0 = buildOverflowIntLiteral(0)
    val overflowInt100 = buildOverflowIntLiteral(100)
    Apply(
      Ident(termName("require")),
      List(
        InfixOp(
          InfixOp(termIdent(n), termIdent(">="), overflowInt0),
          termIdent("&&"),
          InfixOp(termIdent(n), termIdent("<="), overflowInt100)
        )
      )
    )
  }

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    tree match {
      /* Import */
      // add import statements related to Assn1, remove framework and fake exercises definition
      case PackageDef(pid: Ident, stats) if pid.name.toString == "<empty>" => {
        val importFramework = buildImport("epl.assn1.framework._")
        val importFakeExs = fakeExercises.map { exer =>
          buildImport(s"epl.assn1.fake.${exer}")
        }.toList
        val newStats = importFramework :: importFakeExs ++ stats.flatMap {
          // remove the original framework and specific exercises
          case TypeDef(name, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          case DefDef(name, _, _, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          case ModuleDef(name, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          case ValDef(name, _, _) if framework.contains(name.toString) || fakeExercises.contains(name.toString) =>
            Nil
          case other => List(other)
        }

        val newPackageName = Utils.extractFileName(dottyCtx.source.toString)
        super.transform(cpy.PackageDef(tree)(termIdent(newPackageName), newStats))
      }


      /* translateDouble */
      case Ident(name) if name.toString == "Double" && translateDouble =>
        name match {
          case name if name.isTermName => termIdent("BigInt")
          case name if name.isTypeName => typeIdent("BigInt")
        }

      case Number(digits, _) if translateDouble => {
        if (digits.toDouble == digits.toDouble.toInt)
          buildBigIntLiteral(digits.toDouble.toInt)
        else
          sys.error("Unable to translate floating-point numbers to BigInt number.")
      }

      // No way to add .abs for BigInt in Stainless library...
      case Select(qualifier, name) if name.toString == "abs" && translateDouble =>
        Apply(Select(Select(termIdent("stainless"), termName("math")), termName("abs")), List(qualifier))


      /* Use Map instead of ListMap */
      // Handling ListMap initialization
      case Apply(fun, args) if useMap && (fun.isInstanceOf[Ident] && fun.asInstanceOf[Ident].name.toString == "ListMap"
        || fun.isInstanceOf[Select] && fun.asInstanceOf[Select].toString.endsWith("ListMap)")
        || fun.isInstanceOf[TypeApply] && fun.asInstanceOf[TypeApply].toString.contains("ListMap")) && args.nonEmpty =>
        Apply(Ident(termName("Map")), super.transform(args))

      case Select(Select(Select(Ident(name1), name2), name3), name4) if useMap && s"$name1.$name2.$name3.$name4" == "scala.collection.immutable.ListMap" =>
        buildSelect("stainless.lang.Map")

      case Ident(name) if useMap && name.toString == "ListMap" =>
        name match {
          case name if name.isTermName => termIdent("Map")
          case name if name.isTypeName => typeIdent("Map")
        }

      // Solve part of "Can't extract map union with non-finite map" restriction for list2map and election exercises.
      // scala.collection.immutable.ListMap(k -> v) ++ list2map(xs) -> list2map(xs) + Map(k -> v)
      case InfixOp(left, op@Ident(opName), right@Apply(Ident(funName), args)) if useMap && opName == termName("++") && (funName == termName("list2map") || funName == termName("election")) =>
        InfixOp(transform(right), op, transform(left))


      /* Add precondition for sum, suffix and p */
      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "sum" || name.toString == "suffix" => {
        val precondition = getPrecondition("n")
        val newRhs = defDef.rhs match {
          case Block(stats, expr) => Block(precondition :: stats, expr)
          // The function body originally only had one statement. Wrap it in a block.
          case _ => Block(List(precondition), defDef.rhs)
        }
        super.transform(cpy.DefDef(defDef)(name, paramss, tpt, newRhs))
      }

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "p" => {
        val precondition_x = getPrecondition("x")
        val precondition_y = getPrecondition("y")

        val newRhs = defDef.rhs match {
          case Block(stats, expr) => Block(precondition_x :: precondition_y :: stats, expr)
          case _ =>
            // The function body originally only had one statement. Wrap it in a block.
            Block(List(precondition_x, precondition_y), defDef.rhs)
        }
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))
      }


      /* Add return type for some exercises without explicit return type */
      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "mayOverlap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), typeIdent("Boolean"), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose1" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), typeIdent("C"), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), Parens(Function(List(typeIdent("A")), typeIdent("C"))), transform(defDef.rhs)))

      case valDef@ValDef(name, tpt, _) if name.toString == "presidentListMap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.ValDef(tree)(name, AppliedTypeTree(typeIdent("ListMap"), List(typeIdent("OverflowInt"), typeIdent("StringWrapper"))), transform(valDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "map12_withUpdate" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), AppliedTypeTree(typeIdent("ListMap"), List(typeIdent("OverflowInt"), typeIdent("StringWrapper"))), transform(defDef.rhs)))

      case _ => super.transform(tree)
    }
}
