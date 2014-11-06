package org.allenai.nlpstack.webapp.tools

import org.allenai.nlpstack.core.{ Tokenizer, Token, Writer }
import org.allenai.nlpstack.webapp.Whatswrong._

import java.awt.image.BufferedImage

object TokenizerTool extends Tool("tokenize") with StringFormat {
  type Output = Seq[Token]

  override def info = ToolInfo(Impl.tokenizer.getClass.getSimpleName, Impl.obamaSentences)

  override def split(input: String) = input split "\n"
  override def process(section: String) = Impl.tokenizer(section)
  override def visualize(output: Output) = {
    Seq(
      implicitly[Writer[Output, BufferedImage]].write(output)
    )
  }
  override def stringFormat = Tokenizer.multilineStringFormat
}
