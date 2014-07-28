package org.allenai.nlpstack.lemmatize

import org.allenai.common.testkit.UnitSpec

class MorphaLemmatizerSpec extends UnitSpec {
  "lemmatizer" should "correctly lemmatize a word" in {
    val word = "ate"
    val lemma = MorphaStemmer.lemmatize(word)
    assert(lemma === "eat")
  }

  it should "not lemmatize a word with spaces" in {
    val wordWithSpace = "29 1/2"
    assert(MorphaStemmer.lemmatize(wordWithSpace) === wordWithSpace)
  }
}

