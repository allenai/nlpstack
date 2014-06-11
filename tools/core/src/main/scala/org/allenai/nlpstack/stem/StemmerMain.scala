package org.allenai.nlpstack.lemmatize

import org.allenai.nlpstack.LineProcessor

abstract class StemmerMain
    extends LineProcessor("stemmer") {
  def lemmatizer: Stemmer
  override def process(line: String) = line.split("\\s+").map(lemmatizer.stem(_)).mkString(" ")
}
