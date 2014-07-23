package org.allenai.nlpstack.core

case class Lemmatized[+T <: Token](token: T, lemma: String)
