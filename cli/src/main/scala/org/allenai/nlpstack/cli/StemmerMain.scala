package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.core.lemmatize._
import org.allenai.nlpstack.lemmatize.MorphaStemmer

abstract class StemmerMain
    extends LineProcessor("stemmer") {
  def lemmatizer: Stemmer
  override def process(line: String) = line.split("\\s+").map(lemmatizer.stem(_)).mkString(" ")
}

object MorphaStemmerMain extends StemmerMain {
  lazy val lemmatizer = new MorphaStemmer
}
