package org.allenai.aitk.nlpweb.tools

import org.allenai.aitk.parse.graph.DependencyGraph
import org.allenai.aitk.nlpweb.Whatswrong._
import org.allenai.aitk.Writer
import java.awt.image.BufferedImage

object DependencyParserTool extends Tool("dependencies") with StringFormat {
  type Output = DependencyGraph

  override def info = ToolInfo(Impl.dependencyParser.getClass.getSimpleName, Impl.obamaSentences)

  override def split(input: String) = input split "\n"
  override def process(section: String) = {
    val tokens = Impl.tokenizer(section)
    val postags = Impl.postagger.postagTokenized(tokens)
    Impl.dependencyParser.dependencyGraphPostagged(postags)
  }
  override def visualize(output: Output) = {
    Seq(
      implicitly[Writer[Output, BufferedImage]].write(output)
    )
  }
  override def stringFormat = DependencyGraph.multilineStringFormat
}
