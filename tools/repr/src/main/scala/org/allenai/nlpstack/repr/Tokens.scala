package org.allenai.nlpstack.repr

import org.allenai.nlpstack.core.Token

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

  def tokenizer: org.allenai.nlpstack.core.Tokenizer

  override lazy val tokens: Seq[Token] =
    tokenizer.tokenize(text)
}
