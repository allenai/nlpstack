package org.allenai.aitk
package segment

import org.allenai.aitk.tokenize.Tokenizer
import org.allenai.aitk.tokenize.Token

import chalk.text.segment.JavaSentenceSegmenter

class ChalkSentencer extends Segmenter {
  val sentencer = new JavaSentenceSegmenter()

  override def segmentTexts(document: String) = {
    sentencer(document)
  }

  def segment(document: String) = {
    Tokenizer.computeOffsets(segmentTexts(document), document).map {
      case Token(string, offset) => Segment(string, offset)
    }
  }
}

object ChalkSentencerMain
    extends SegmenterMain {
  lazy val sentencer = new ChalkSentencer
}
