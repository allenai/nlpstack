package org.allenai.aitk
package segment

abstract class SegmenterMain
    extends LineProcessor("segmenter") {
  def sentencer: Segmenter
  override def process(line: String) =
    sentencer(line).mkString("\n")
}
