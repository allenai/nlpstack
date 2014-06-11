package org.allenai.repr.sentence

import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.chunk._
import org.allenai.nlpstack.lemmatize._

trait TokensSupertrait {
  this: Sentence =>
  type token <: Token

  def tokens: Seq[token]

  def strings: Seq[String] = tokens.map(_.string)
}

trait Tokens extends TokensSupertrait {
  this: Sentence =>
  type token = Token
}

trait Tokenizer extends Tokens {
  this: Sentence =>

  def tokenizer: org.allenai.nlpstack.tokenize.Tokenizer

  override lazy val tokens: Seq[Token] =
    tokenizer.tokenize(text)
}
