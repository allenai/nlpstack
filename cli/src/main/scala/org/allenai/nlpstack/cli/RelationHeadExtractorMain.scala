package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.core.headword.HeadExtractor
import org.allenai.nlpstack.headword.KnowitallHeadExtractor
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

abstract class RelationHeadExtractorMain extends LineProcessor("relationheadextractor") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  def headExtractor: HeadExtractor

  override def process(line: String) = {
    val headTokens = headExtractor.relationHead(tokenizer, postagger)(line)
    Postagger.multilineStringFormat.write(headTokens)
  }
}

object KnowitallRelationHeadExtractorMain extends RelationHeadExtractorMain {
  override val tokenizer = defaultTokenizer
  override val postagger = defaultPostagger
  override val headExtractor = new KnowitallHeadExtractor()
}