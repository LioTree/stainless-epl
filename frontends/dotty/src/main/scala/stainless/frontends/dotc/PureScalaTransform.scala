package stainless.frontends.dotc

import dotty.tools.dotc._
import core._
import Phases._
import ast.Trees._
import Contexts._

// Similar to `MacroTransform`, but we need to handle `untpdTree`,
// so we cannot directly inherit from it.
class PureScalaTransform extends Phase {

  import ast.untpd._

  override val phaseName = "pure Scala Transform"

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.untpdTree = atPhase(transformPhase)(newTransformer.transform(unit.untpdTree))
  }

  protected def newTransformer(using Context): PureScalaTransformer = new PureScalaTransformer

  protected def transformPhase(using Context): Phase = this

  class PureScalaTransformer extends UntypedTreeMap {
    override def transform(tree: Tree)(using Context): Tree = {
      import Names.termName
      import Names.typeName
      tree match {
        // Replace Double with BigInt.
        case Ident(name) if name.toString == "Double" =>
          Ident(typeName("BigInt"))
        // Replace Int with BigInt.
        case Ident(name) if name.toString == "Int" =>
          Ident(typeName("BigInt"))
        // Replace Nil with Nil().
        case Ident(name) if name.toString == "Nil" =>
          Apply(Ident(termName("Nil")), Nil)
        // Add `import stainless.collection._` `import stainless.annotation._` and
        // `import stainless.lang._` to the beginning of the file.
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
          cpy.PackageDef(tree)(transformSub(pid), importCollection :: importAnnotation :: importLang :: transformStats(stats, ctx.owner))
        case _ =>
          super.transform(tree)
      }
    }
  }
}
