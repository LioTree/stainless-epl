package stainless.frontends.dotc.purescala

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.NumberKind.{Decimal, Whole}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.{EmptyTypeName, TermName, TypeName, termName, typeName}
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable.{ArrayBuffer, Set, Stack}

class Assignment1Transformer(extractPublicClass: Boolean, publicPackageName: String, targets: Set[String]) extends PureScalaTransformer(targets) {
  import ast.untpd.*
  
  private val publicClasses = List(
    "Colour",
    "Red",
    "Green",
    "Blue",
    "Shape",
    "Circle",
    "Rectangle",
  )

  override def transform(tree: Tree)(using Context): Tree = {
    tree match {
      case Ident(name) if publicClasses.contains(name.toString) => Select(Ident(termName(publicPackageName)), name)

      case TypeDef(name, rhs) if (!extractPublicClass && publicClasses.contains(name.toString)) => EmptyTree

      case _ => super.transform(tree)
    }
  }
}
