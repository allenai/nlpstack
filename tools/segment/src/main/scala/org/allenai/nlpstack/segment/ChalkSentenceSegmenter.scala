package org.allenai.nlpstack
package segment

@deprecated("Please use defaultSegmenter instead", "2014-06-24")
class ChalkSentenceSegmenter extends FactorieSegmenter

object ChalkSentencerMain extends SegmenterMain {
  lazy val sentencer = new ChalkSentenceSegmenter
}
