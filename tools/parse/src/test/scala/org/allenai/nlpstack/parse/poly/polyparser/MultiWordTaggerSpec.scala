package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, Token }

class MultiWordTaggerSpec extends UnitSpec {
  // scalastyle:off

  import MultiWordTagger.{ mweSymbol, symbolFor, mweValue }

  val string = "I like pizza because of the Ninja Turtles"
  val sentence = Sentence(string.split(" ").map(x => Token(Symbol(x))))
  val mwe1 = IndexedSeq('pizza, 'because, 'of)
  val mwe2 = IndexedSeq('because, 'of)
  val mwe3 = IndexedSeq('I, 'like)
  val mwe4 = IndexedSeq('Ninja, 'Turtles)
  val mwe5 = IndexedSeq('Turtles)
  val dictionary = Set(mwe1, mwe2, mwe3, mwe4, mwe5)
  //val tagger = MultiWordTagger(dictionary)
  //val got = tagger(sentence)

  // Empty property map
  val propNone = Map.empty[Symbol, String]

  // Property map containing "part of mwe" property
  val mweProp = Map(mweSymbol -> mweValue)

  // Expected property map for a token in the given mwe
  def propFor(mwe: IndexedSeq[Symbol]) = mweProp + (symbolFor(mwe) -> mweValue)

  /*
  "MultiWordTagger" should "predict properties correctly" in {
    val expected = Seq(
      propFor(mwe3), // I
      propFor(mwe3), // like
      propFor(mwe1), // pizza
      propFor(mwe1) ++ propFor(mwe2), // because
      propFor(mwe1) ++ propFor(mwe2), // of
      propNone, // the
      propFor(mwe4), // Ninja
      propFor(mwe4) ++ propFor(mwe5)) // Turtles
    val predicted = tagger(sentence).tokens.map(_.properties)
    assert(expected == predicted)
  }
  */
}
