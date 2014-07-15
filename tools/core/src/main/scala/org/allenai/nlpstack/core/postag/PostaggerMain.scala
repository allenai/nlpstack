package org.allenai.nlpstack.core.postag

import org.allenai.nlpstack.core.LineProcessor

import org.allenai.nlpstack.core.tokenize._

abstract class PostaggerMain extends LineProcessor("postagger") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  override def process(line: String) = {
    val postaggedTokens = postagger.postag(tokenizer)(line)
    Postagger.multilineStringFormat.write(postaggedTokens)
  }
}
