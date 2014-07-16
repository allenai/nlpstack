package org.allenai.nlpstack.lemmatize

import org.allenai.nlpstack.core.lemmatize.{ PostaggedStemmer, Stemmer, StemmerMain }

import edu.washington.cs.knowitall.morpha.{ MorphaStemmer => MorphaStem }

/** This stemmer handles many cases, but the JFlex is 5 MB. */
class MorphaStemmer extends Stemmer with PostaggedStemmer {
  def stem(word: String) = MorphaStem.stemToken(word)

  override def stem(word: String, postag: String) = MorphaStem.stemToken(word, postag)
}

/** MorphaStemmer is threadsafe.  Clients can use this global instance. */
object MorphaStemmer extends MorphaStemmer

object MorphaStemmerMain extends StemmerMain {
  lazy val lemmatizer = new MorphaStemmer
}
