package org.allenai.nlpstack
package lemmatize

abstract class StemmerMain
    extends LineProcessor("stemmer") {
  def lemmatizer: Stemmer
  override def process(line: String) = line.split("\\s+").map(lemmatizer.stem(_)).mkString(" ")
}
