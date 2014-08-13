package org.allenai.nlpstack.repr

import org.allenai.nlpstack.core.PostaggedToken

trait PostagsSupertrait extends TokensSupertrait {
  this: Sentence =>

  type token <: PostaggedToken

  def postags: Seq[String] = tokens.map(_.postag)
}

trait Postags extends PostagsSupertrait {
  this: Sentence =>

  type token = PostaggedToken
}

trait Postagger extends Postags {
  this: Sentence =>
  def tokenizer: org.allenai.nlpstack.core.Tokenizer
  def postagger: org.allenai.nlpstack.core.Postagger

  override lazy val tokens: Seq[PostaggedToken] =
    postagger.postag(tokenizer)(this.text)
}

