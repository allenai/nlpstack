package org.allenai.repr.sentence

import org.allenai.common.immutable.Interval
import org.allenai.nlpstack.chunk._

trait ChunksSupertrait extends PostagsSupertrait {
  this: Sentence =>

  type token <: ChunkedToken

  def chunks: Seq[String] = tokens.map(_.chunk)
  def chunkIntervals: Seq[(String, Interval)] = Chunker.intervals(tokens)
}

trait Chunks extends ChunksSupertrait {
  this: Sentence =>

  type token = ChunkedToken
}

trait Chunker extends Chunks {
  this: Sentence =>

  def tokenizer: org.allenai.nlpstack.tokenize.Tokenizer
  def postagger: org.allenai.nlpstack.postag.Postagger
  def chunker: org.allenai.nlpstack.chunk.Chunker

  override lazy val tokens: Seq[ChunkedToken] =
    chunker.chunk(tokenizer, postagger)(this.text)
}
