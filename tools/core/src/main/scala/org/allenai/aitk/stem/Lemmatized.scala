package org.allenai.aitk.lemmatize

import org.allenai.aitk.tokenize.Token

case class Lemmatized[+T <: Token](token: T, lemma: String)
