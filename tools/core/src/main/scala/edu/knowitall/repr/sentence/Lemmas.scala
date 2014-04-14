package org.allenai.repr.sentence

import org.allenai.aitk.stem._

trait Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizedTokens: Seq[org.allenai.aitk.stem.Lemmatized[token]]
}

trait Lemmatizer extends Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizer: Stemmer

  override lazy val lemmatizedTokens: Seq[org.allenai.aitk.stem.Lemmatized[token]] =
    tokenized.tokens map lemmatizer.lemmatizeToken
}
