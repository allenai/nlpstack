package org.allenai.repr.sentence

import org.allenai.nlpstack.lemmatize._

trait Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizedTokens: Seq[org.allenai.nlpstack.lemmatize.Lemmatized[token]]
}

trait Lemmatizer extends Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizer: Stemmer

  override lazy val lemmatizedTokens: Seq[org.allenai.nlpstack.lemmatize.Lemmatized[token]] =
    tokenized.tokens map lemmatizer.lemmatizeToken
}
