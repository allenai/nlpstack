package org.allenai.nlpstack.core.repr

import org.allenai.common.immutable.Interval
import org.allenai.nlpstack.core.ChunkedToken

trait ChunksSupertrait extends PostagsSupertrait {
  this: Sentence =>

  type token <: ChunkedToken

  def chunks: Seq[String] = tokens.map(_.chunk)
  def chunkIntervals: Seq[(String, Interval)] =
    org.allenai.nlpstack.core.Chunker.intervals(tokens)
}

trait Chunks extends ChunksSupertrait {
  this: Sentence =>

  type token = ChunkedToken
}

trait Chunker extends Chunks {
  this: Sentence =>

  def tokenizer: org.allenai.nlpstack.core.Tokenizer
  def postagger: org.allenai.nlpstack.core.Postagger
  def chunker: org.allenai.nlpstack.core.Chunker

  override lazy val tokens: Seq[ChunkedToken] =
    chunker.chunk(tokenizer, postagger)(this.text)
}
