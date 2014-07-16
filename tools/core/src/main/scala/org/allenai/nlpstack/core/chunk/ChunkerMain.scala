package org.allenai.nlpstack.core.chunk

import org.allenai.nlpstack.core.LineProcessor
import org.allenai.nlpstack.core.postag._
import org.allenai.nlpstack.core.tokenize._

abstract class ChunkerMain
    extends LineProcessor("chunker") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  def chunker: Chunker

  override def process(line: String) = {
    val chunkedTokens = chunker.chunk(tokenizer, postagger)(line)
    Chunker.multilineStringFormat.write(chunkedTokens)
  }

  override def init(config: Config) {
    // for timing purposes
    chunker.chunk(tokenizer, postagger)("I want to initialize the chunker.")
  }
}
