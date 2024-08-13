package stainless
package epl
import inox.Context
import stainless.extraction.StainlessPipeline

object DebugSectionTransformation extends inox.DebugSection("transformation")

object optAssn1 extends inox.FlagOptionDef("assn1", true)

object optAssn2 extends inox.FlagOptionDef("assn2", true)

object optTransformation extends inox.FlagOptionDef("transformation", true)

object optGenSubFuns extends inox.FlagOptionDef("gen-subfns", true)

object optFakeExercises extends inox.OptionDef[Seq[String]] {
  val name = "fake-exs"
  val default = Seq[String]()
  val parser = inox.OptionParsers.seqParser(inox.OptionParsers.stringParser)
  val usageRhs = "f1,c2,o3..."
}

object optExtractTarget extends inox.OptionDef[Seq[String]] {
  val name = "extract"
  val default = Seq[String]()
  val parser = inox.OptionParsers.seqParser(inox.OptionParsers.stringParser)
  val usageRhs = "f1,c2,o3..."
}