package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.LineProcessor

abstract class TokenizerMain extends LineProcessor("tokenizer") {
  def tokenizer: Tokenizer
  override def process(sentence: String) =
    Tokenizer.multilineStringFormat.write(tokenizer.tokenize(sentence))
}
