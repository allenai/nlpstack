package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.core.tokenize._
import org.allenai.nlpstack.core.postag._
import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._

abstract class PostaggerMain extends LineProcessor("postagger") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  override def process(line: String) = {
    val postaggedTokens = postagger.postag(tokenizer)(line)
    Postagger.multilineStringFormat.write(postaggedTokens)
  }
}

object FactoriePostaggerMain extends PostaggerMain {
  override val tokenizer = defaultTokenizer
  override val postagger = new FactoriePostagger()
}
