package stainless
package cluster
import inox.Context
import stainless.extraction.StainlessPipeline

object optFrameworkFile extends inox.OptionDef[Seq[String]] {
  val name = "framework-file"
  val default = Seq[String]()
  val usageRhs = "file1,file2,..."
  val parser = inox.OptionParsers.seqParser(inox.OptionParsers.stringParser)
}

//object ClusterComponent extends Component {
//  override val name = "cluster"
//  override val description = "Cluster assignments"
//}