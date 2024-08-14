package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.epl.{optAssn1, optExtractTarget, optFakeExercises}

class Assn1Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends PackageNameRewriter {

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
  private val extractTargets = inoxCtx.options.findOption(optExtractTarget) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }
  private val fakeExercises = inoxCtx.options.findOption(optFakeExercises) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }
  private val translateDouble: Boolean = extractTargets.contains("boundingBox") || extractTargets.contains("mayOverlap")
  private val useMap: Boolean = extractTargets.contains("reverse") || extractTargets.contains("list2map") || extractTargets.contains("election")

  override def start(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    inoxCtx.options.findOption(optAssn1) match {
      case Some(true) => transform(tree)
      case _ => tree
    }

  private def getPrecondition(n: String): Apply = {
    val overflowInt0 = Apply(termIdent("OverflowInt"), List(Apply(termIdent("BigInt"), List(buildNumber(0)))))
    val overflowInt100 = Apply(termIdent("OverflowInt"), List(Apply(termIdent("BigInt"), List(buildNumber(100)))))
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

        super.transform(cpy.PackageDef(tree)(pid, newStats))
      }

      case Ident(name) if name.toString == "Double" && translateDouble =>
        name match {
          case name if name.isTermName => termIdent("BigInt")
          case name if name.isTypeName => typeIdent("BigInt")
        }

      case Apply(Ident(name), List(Apply(Ident(name2), List(num:Number)))) if translateDouble && name == termName("OverflowInt") && name2 == termName("BigInt") =>
        Apply(Ident(termName("BigInt")), List(transform(num)))

      case Number(digits, _) if digits.contains(".") && translateDouble => {
        if (digits.toDouble == digits.toDouble.toInt)
          buildNumber(digits.toDouble.toInt)
        else
          sys.error("Unable to translate floating-point numbers with decimals.")
      }

      // No way to add .abs for BigInt in Stainless library...
      case Select(qualifier, name) if name.toString == "abs" && translateDouble =>
        Apply(Select(Select(termIdent("stainless"), termName("math")), termName("abs")), List(qualifier))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "sum" || name.toString == "suffix" => {
        val precondition = getPrecondition("n")
        val newRhs = defDef.rhs match {
          case Block(stats, expr) => Block(precondition :: stats, expr)
          // The function body originally only had one statement. Wrap it in a block.
          case _ => Block(List(precondition), defDef.rhs)
        }
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))
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

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "mayOverlap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), typeIdent("Boolean"), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose1" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), typeIdent("C"), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), Parens(Function(List(typeIdent("A")), typeIdent("C"))), transform(defDef.rhs)))

      case valDef@ValDef(name, tpt, _) if name.toString == "presidentListMap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.ValDef(tree)(name, AppliedTypeTree(typeIdent("ListMap"), List(typeIdent("Int"), typeIdent("String"))), transform(valDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "map12_withUpdate" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), AppliedTypeTree(typeIdent("ListMap"), List(typeIdent("Int"), typeIdent("String"))), transform(defDef.rhs)))

      case _ => super.transform(tree)
    }
}
