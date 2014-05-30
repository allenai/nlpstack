package org.allenai.nlpstack
package segment

import org.allenai.nlpstack.tokenize.Tokenizer
import org.allenai.nlpstack.tokenize.Token

import chalk.text.segment.JavaSentenceSegmenter

class ChalkSentenceSegmenter extends Segmenter {
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
  lazy val sentencer = new ChalkSentenceSegmenter
}
