package org.allenai
package aitk
package stem

import aitk.tokenize.Token

case class Lemmatized[+T <: Token](token: T, lemma: String)
