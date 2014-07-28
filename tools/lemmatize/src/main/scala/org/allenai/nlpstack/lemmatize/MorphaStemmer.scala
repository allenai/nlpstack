package org.allenai.nlpstack.lemmatize

import org.allenai.nlpstack.core.{ PostaggedStemmer, Stemmer }

import edu.washington.cs.knowitall.morpha.{ MorphaStemmer => MorphaStem }

/** This stemmer handles many cases, but the JFlex is 5 MB. */
class MorphaStemmer extends Stemmer with PostaggedStemmer {
  private val whitespace = "\\s".r

  private def stem(word: String, stemmer: (String => String)) =
    if (whitespace.findFirstMatchIn(word).isDefined)
      word
    else
      stemmer(word)

  def stem(word: String) = stem(word, MorphaStem.stemToken(_))
  override def stem(word: String, postag: String) =
    stem(word, MorphaStem.stemToken(_, postag))
}

/** MorphaStemmer is threadsafe.  Clients can use this global instance. */
object MorphaStemmer extends MorphaStemmer
