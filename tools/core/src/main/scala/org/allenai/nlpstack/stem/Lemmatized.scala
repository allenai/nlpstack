package org.allenai.nlpstack.lemmatize

import org.allenai.nlpstack.tokenize.Token

case class Lemmatized[+T <: Token](token: T, lemma: String)
