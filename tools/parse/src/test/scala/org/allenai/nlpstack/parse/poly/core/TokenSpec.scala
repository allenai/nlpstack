package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.immutable.Interval
import org.allenai.common.testkit.UnitSpec


object TokenSpecTestData {
  // scalastyle:off

  def propertyMap(initialMap: Map[Symbol, String]): Map[Symbol, Set[Symbol]] = {
    initialMap map { case (x, y) => (x, Set(Symbol(y))) }
  }

  val tokens1 = Vector(NexusToken, Token('we, propertyMap(Map('cpos -> "NOUN", 'pos -> "NN"))),
    Token('saw, propertyMap(Map('cpos -> "VERB", 'pos -> "VB"))),
    Token('black, propertyMap(Map('cpos -> "ADJ", 'pos -> "JJ"))),
    Token('cats, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))))

  val sent1 = Sentence(TokenSpecTestData.tokens1)

  val tokens2 = Vector(NexusToken, Token('with, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('the, propertyMap(Map('cpos -> "DET", 'pos -> "DT"))),
    Token('help, propertyMap(Map('cpos -> "NOUN", 'pos -> "NN"))),
    Token('of, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('animals, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token('insects, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token('and, propertyMap(Map('cpos -> "CONJ", 'pos -> "CC"))),
    Token('birds, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token('flowers, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token('can, propertyMap(Map('cpos -> "VERB", 'pos -> "MD"))),
    Token('be, propertyMap(Map('cpos -> "VERB", 'pos -> "VB"))),
    Token('pollinated, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token('fertilized, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token(Symbol("."), propertyMap(Map('cpos -> ".", 'pos -> "."))))

  val sent2 = Sentence(TokenSpecTestData.tokens2)

  val tokens3 = Vector(NexusToken, Token('with, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('the, propertyMap(Map('cpos -> "DET", 'pos -> "DT"))),
    Token('help, propertyMap(Map('cpos -> "NOUN", 'pos -> "NN"))),
    Token('of, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('animals, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token('insects, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token('and, propertyMap(Map('cpos -> "CONJ", 'pos -> "CC"))),
    Token('birds, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token('flowers, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token('can, propertyMap(Map('cpos -> "VERB", 'pos -> "MD"))),
    Token('be, propertyMap(Map('cpos -> "VERB", 'pos -> "VB"))),
    Token('pollinated, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token('fertilized, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token(Symbol("."), propertyMap(Map('cpos -> ".", 'pos -> "."))))

  val sent3 = Sentence(TokenSpecTestData.tokens3)

  val tokens4 = Vector(NexusToken, Token('with, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('the, propertyMap(Map('cpos -> "DET", 'pos -> "DT"))),
    Token('help, propertyMap(Map('cpos -> "NOUN", 'pos -> "NN"))),
    Token('of, propertyMap(Map('cpos -> "ADP", 'pos -> "IN"))),
    Token('animals, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token('insects, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token('and, propertyMap(Map('cpos -> "CONJ", 'pos -> "CC"))),
    Token('birds, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token(Symbol(")"), propertyMap(Map('cpos -> ".", 'pos -> "-RRB-"))),
    Token('flowers, propertyMap(Map('cpos -> "NOUN", 'pos -> "NNS"))),
    Token('can, propertyMap(Map('cpos -> "VERB", 'pos -> "MD"))),
    Token('be, propertyMap(Map('cpos -> "VERB", 'pos -> "VB"))),
    Token('pollinated, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol("("), propertyMap(Map('cpos -> ".", 'pos -> "-LRB-"))),
    Token('fertilized, propertyMap(Map('cpos -> "VERB", 'pos -> "VBN"))),
    Token(Symbol("."), propertyMap(Map('cpos -> ".", 'pos -> "."))))

  val sent4 = Sentence(TokenSpecTestData.tokens4)
}

class TokenSpec extends UnitSpec {
  // scalastyle:off

  /*
  "Initializing a sentence" should "give the correct lookAhead" in {
    TokenSpecTestData.sent1.lookAhead(1).get('cpos) shouldBe
      Some(Set("NOUN", "VERB", "ADJ"))
    TokenSpecTestData.sent1.lookAhead(1).get('pos) shouldBe
      Some(Set("VB", "JJ", "NNS"))
    TokenSpecTestData.sent1.lookAhead(4).get('cpos) shouldBe
      Some(Set())
  }

  it should "give the correct lookBehind" in {
    TokenSpecTestData.sent1.lookBehind(3).get('cpos) shouldBe
      Some(Set("nexus", "VERB", "NOUN"))
    TokenSpecTestData.sent1.lookBehind(3).get('pos) shouldBe
      Some(Set("nexus", "VB", "NN"))
    TokenSpecTestData.sent1.lookBehind(0).get('cpos) shouldBe
      Some(Set())
  }
  */

  it should "give the correct paren intervals for sent1" in {
    TokenSpecTestData.sent1.parenIntervals shouldBe
      Set.empty
  }

  it should "give the correct paren intervals for sent2" in {
    TokenSpecTestData.sent2.parenIntervals shouldBe
      Set(Interval.closed(6, 10), Interval.closed(15, 17))
  }

  it should "give the correct paren intervals for sent3" in {
    TokenSpecTestData.sent3.parenIntervals shouldBe
      Set(Interval.closed(7, 9), Interval.closed(6, 12), Interval.closed(17, 19))
  }

  it should "give the correct paren intervals for sent4" in {
    TokenSpecTestData.sent4.parenIntervals shouldBe
      Set(Interval.closed(0, 7), Interval.closed(0, 10), Interval.closed(15, 17))
  }



}
