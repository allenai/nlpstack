package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core.Tokenizer
import org.allenai.nlpstack.tokenize._

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

object Ai2TokenizerMain extends TokenizerMain {
  val tokenizer = Ai2Tokenizer
}

object StanfordTokenizerMain extends TokenizerMain {
  val tokenizer = StanfordTokenizer
}
