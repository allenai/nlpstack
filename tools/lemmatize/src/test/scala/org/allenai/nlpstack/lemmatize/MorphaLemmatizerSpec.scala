package org.allenai.nlpstack.lemmatize

import org.allenai.common.testkit.UnitSpec

class MorphaLemmatizerSpec extends UnitSpec {
  "lemmatizer" should "correctly lemmatize a word" in {
    val word = "ate"
    val lemma = MorphaStemmer.lemmatize(word)
    lemma === "eat"
  }
}

