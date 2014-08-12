package org.allenai.nlpstack.core.repr

import org.allenai.nlpstack.core.Stemmer

trait Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizedTokens: Seq[org.allenai.nlpstack.core.Lemmatized[token]]
}

trait Lemmatizer extends Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizer: Stemmer

  override lazy val lemmatizedTokens: Seq[org.allenai.nlpstack.core.Lemmatized[token]] =
    tokenized.tokens map lemmatizer.lemmatizeToken
}
