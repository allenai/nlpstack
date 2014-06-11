package org.allenai.nlpstack.segment

import org.allenai.nlpstack.LineProcessor

abstract class SegmenterMain
    extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) =
    sentencer(line).mkString("\n")
}
