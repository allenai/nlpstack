package org.allenai.nlpstack.nlpweb.tools

import org.allenai.nlpstack.nlpweb.Whatswrong
import org.allenai.nlpstack.Writer
import java.awt.image.BufferedImage
import org.allenai.nlpstack.segment._

object SentenceSegmenterTool extends Tool("segment") {
  type Output = Seq[Segment]

  override def info = ToolInfo(Impl.sentenceSegmenter.getClass.getSimpleName, Impl.obamaText)

  override def split(input: String) = Seq(input)
  override def process(section: String) = Impl.sentenceSegmenter(section).toSeq
  override def visualize(output: Output) = Seq.empty
  override def format(output: Output) = Seq(output mkString "\n")
}