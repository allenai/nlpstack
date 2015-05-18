package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.segment.{ StanfordSegmenter, ChalkSentenceSegmenter, FactorieSegmenter }

abstract class SegmenterMain
    extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) = sentencer(line).map(_.text).mkString("\n")
}

object FactorieSegmenterMain extends SegmenterMain {
  override val sentencer = new FactorieSegmenter()
}

object StanfordSegmenterMain extends SegmenterMain {
  override val sentencer = StanfordSegmenter
}
