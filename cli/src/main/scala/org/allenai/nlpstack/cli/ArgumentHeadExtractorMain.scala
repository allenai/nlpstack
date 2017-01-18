package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.core.headword.HeadExtractor
import org.allenai.nlpstack.headword.KnowitallHeadExtractor
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

abstract class ArgumentHeadExtractorMain extends LineProcessor("argumentheadextractor") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  def headExtractor: HeadExtractor

  override def process(line: String) = {
    val headTokens = headExtractor.argumentHead(tokenizer, postagger)(line)
    Postagger.multilineStringFormat.write(headTokens)
  }
}

object KnowitallArgumentHeadExtractorMain extends ArgumentHeadExtractorMain {
  override val tokenizer = defaultTokenizer
  override val postagger = defaultPostagger
  override val headExtractor = new KnowitallHeadExtractor()
}