package org.allenai.nlpstack.core.segment

import org.allenai.nlpstack.core.LineProcessor

abstract class SegmenterMain
    extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) =
    sentencer(line).mkString("\n")
}
