package org.allenai.nlpstack.webapp.tools

import org.allenai.nlpstack.webapp.Whatswrong
import org.allenai.nlpstack.Writer
import java.awt.image.BufferedImage
import org.allenai.nlpstack.tokenize._

object TokenizerTool extends Tool("tokenize") with StringFormat {
  type Output = Seq[Token]

  override def info = ToolInfo(Impl.tokenizer.getClass.getSimpleName, Impl.obamaSentences)

  override def split(input: String) = input split "\n"
  override def process(section: String) = Impl.tokenizer(section)
  override def visualize(output: Output) = {
    import Whatswrong._
    Seq(
      implicitly[Writer[Output, BufferedImage]].write(output))
  }
  override def stringFormat = Tokenizer.multilineStringFormat
}
