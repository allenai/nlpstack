package org.allenai.nlpstack.webapp.tools

import org.allenai.nlpstack.core.{ ChunkedToken, Chunker, Writer }
import org.allenai.nlpstack.webapp.Whatswrong._

import java.awt.image.BufferedImage

object ChunkerTool extends Tool("chunk") with StringFormat {
  type Output = Seq[ChunkedToken]

  override def info = ToolInfo(Impl.chunker.getClass.getSimpleName, Impl.obamaSentences)

  override def split(input: String) = input split "\n"
  override def process(section: String) = {
    val tokens = Impl.tokenizer(section)
    val postags = Impl.postagger.postagTokenized(tokens)
    Impl.chunker.chunkPostagged(postags)
  }
  override def visualize(output: Output) = {
    Seq(
      implicitly[Writer[Output, BufferedImage]].write(output))
  }
  override def stringFormat = Chunker.stringFormat
}
