package org.allenai.nlpstack.core.lemmatize

import org.allenai.nlpstack.core.tokenize.Token

case class Lemmatized[+T <: Token](token: T, lemma: String)
