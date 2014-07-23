package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core.Tokenizer
import org.allenai.nlpstack.tokenize.FactorieTokenizer
import org.allenai.nlpstack.tokenize.PennTokenizer
import org.allenai.nlpstack.tokenize.WhitespaceTokenizer

abstract class TokenizerMain extends LineProcessor("tokenizer") {
  def tokenizer: Tokenizer
  override def process(sentence: String) =
    Tokenizer.multilineStringFormat.write(tokenizer.tokenize(sentence))
}

object FactorieTokenizerMain extends TokenizerMain {
  val tokenizer = new FactorieTokenizer()
}

object PennTokenizerMain extends TokenizerMain {
  val tokenizer = PennTokenizer
}

object WhitespaceTokenizerMain extends TokenizerMain {
  val tokenizer = WhitespaceTokenizer
}
