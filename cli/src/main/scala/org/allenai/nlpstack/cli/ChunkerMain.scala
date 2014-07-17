package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.chunk._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._

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

object OpenNlpChunkerMain extends ChunkerMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val chunker = new OpenNlpChunker()
}
