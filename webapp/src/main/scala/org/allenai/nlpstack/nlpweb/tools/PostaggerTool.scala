package org.allenai.nlpstack.nlpweb.tools

import org.allenai.nlpstack.nlpweb.Whatswrong._
import org.allenai.nlpstack.Writer
import java.awt.image.BufferedImage
import org.allenai.nlpstack.postag._

object PostaggerTool extends Tool("postag") with StringFormat {
  type Output = Seq[PostaggedToken]

  override def info = ToolInfo(Impl.postagger.getClass.getSimpleName, Impl.obamaSentences)

  override def split(input: String) = input split "\n"
  override def process(section: String) = {
    val tokens = Impl.tokenizer(section)
    Impl.postagger.postagTokenized(tokens)
  }
  override def visualize(output: Output) = {
    Seq(
      implicitly[Writer[Output, BufferedImage]].write(output)
    )
  }
  override def stringFormat = Postagger.multilineStringFormat
}
