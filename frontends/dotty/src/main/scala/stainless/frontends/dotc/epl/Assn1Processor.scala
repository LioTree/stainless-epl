package stainless.frontends.dotc.epl

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context as DottyContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import stainless.equivchk.{optAssn1, optExtractTarget}

class Assn1Processor(using dottyCtx: DottyContext, inoxCtx: inox.Context) extends PackageNameRewriter {

  import ast.untpd.*

  private val extractTargets = inoxCtx.options.findOption(optExtractTarget) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }
  private val translateDouble: Boolean = extractTargets.contains("boundingBox") || extractTargets.contains("mayOverlap")

  override def start(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    inoxCtx.options.findOption(optAssn1) match {
      case Some(true) => transform(tree)
      case _ => tree
    }

  private def getPrecondition(n: String): Apply =
    Apply(
      Ident(termName("require")),
      List(
        InfixOp(
          InfixOp(Ident(termName(n)), Ident(termName(">=")), buildNumber(0)),
          Ident(termName("&&")),
          InfixOp(Ident(termName(n)), Ident(termName("<=")), buildNumber(100))
        )
      )
    )

  override def transform(tree: untpd.Tree)(using DottyContext): untpd.Tree =
    tree match {
      case Ident(name) if name.toString == "Double" && translateDouble =>
        name match {
          case name if name.isTermName => Ident(termName("BigInt"))
          case name if name.isTypeName => Ident(typeName("BigInt"))
        }

      case Number(digits, _) if translateDouble =>
        if (digits.contains(".")) {
          if (digits.toDouble == digits.toDouble.toInt)
            Apply(Ident(termName("BigInt")), List(Number((digits.toDouble.toInt.toString), Whole(10))))
          else
            sys.error("Unable to translate floating-point numbers with decimals.")
        }
        else
          Apply(Ident(termName("BigInt")), List(tree))

      // No way to add .abs for BigInt in Stainless library...
      case Select(qualifier, name) if name.toString == "abs" && translateDouble =>
        Apply(Select(Select(Ident(termName("stainless")), termName("math")), termName("abs")), List(qualifier))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "sum" || name.toString == "power" || name.toString == "suffix" =>
        val precondition = getPrecondition("n")
        val newRhs = defDef.rhs match {
          case Block(stats, expr) => Block(precondition :: stats, expr)
          // The function body originally only had one statement. Wrap it in a block.
          case _ => Block(List(precondition), defDef.rhs)
        }
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "p" =>
        val precondition_x = getPrecondition("x")
        val precondition_y = getPrecondition("y")

        val newRhs = defDef.rhs match {
          case Block(stats, expr) => Block(precondition_x :: precondition_y :: stats, expr)
          case _ =>
            // The function body originally only had one statement. Wrap it in a block.
            Block(List(precondition_x, precondition_y), defDef.rhs)
        }
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), transform(tpt), transform(newRhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "mayOverlap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), Ident(typeName("Boolean")), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose1" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), Ident(typeName("C")), transform(defDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "compose" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), Parens(Function(List(Ident(typeName("A"))), Ident(typeName("C")))), transform(defDef.rhs)))

      case valDef@ValDef(name, tpt, _) if name.toString == "presidentListMap" && tpt.toString == "TypeTree" =>
        super.transform(cpy.ValDef(tree)(name, AppliedTypeTree(Ident(typeName("ListMap")), List(Ident(typeName("Int")), Ident(typeName("String")))), transform(valDef.rhs)))

      case defDef@DefDef(name, paramss, tpt, _) if name.toString == "map12_withUpdate" && tpt.toString == "TypeTree" =>
        super.transform(cpy.DefDef(defDef)(name, transformParamss(paramss), AppliedTypeTree(Ident(typeName("ListMap")), List(Ident(typeName("Int")), Ident(typeName("String")))), transform(defDef.rhs)))

      case _ => super.transform(tree)
    }
}
