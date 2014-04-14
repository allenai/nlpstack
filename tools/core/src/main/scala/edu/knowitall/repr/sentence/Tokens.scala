package org.allenai.repr.sentence

import org.allenai.aitk.tokenize._
import org.allenai.aitk.postag._
import org.allenai.aitk.chunk._
import org.allenai.aitk.lemmatize._

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

  def tokenizer: org.allenai.aitk.tokenize.Tokenizer

  override lazy val tokens: Seq[Token] =
    tokenizer.tokenize(text)
}
