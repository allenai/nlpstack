package org.allenai.nlpstack.core.lemmatize

import org.allenai.nlpstack.core.LineProcessor

abstract class StemmerMain
    extends LineProcessor("stemmer") {
  def lemmatizer: Stemmer
  override def process(line: String) = line.split("\\s+").map(lemmatizer.stem(_)).mkString(" ")
}
