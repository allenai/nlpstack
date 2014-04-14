package org.allenai.aitk
package tokenize

abstract class TokenizerMain extends LineProcessor("tokenizer") {
  def tokenizer: Tokenizer
  override def process(sentence: String) =
    Tokenizer.multilineStringFormat.write(tokenizer.tokenize(sentence))
}