package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core.segment._
import org.allenai.nlpstack.segment.ChalkSentenceSegmenter
import org.allenai.nlpstack.segment.FactorieSegmenter

abstract class SegmenterMain
    extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) =
    sentencer(line).mkString("\n")
}

object FactorieSegmenterMain extends SegmenterMain {
  val sentencer = new FactorieSegmenter()
}
