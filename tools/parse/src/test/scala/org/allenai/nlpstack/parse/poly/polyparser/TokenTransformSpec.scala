package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, Sentence, NexusToken, Token }
import spray.json._

object TokenTransformTestData {
  // scalastyle:off

  /** This represents the following parser state:
    * format: OFF
    *
    * ---------
    * |        |
    * |        V
    * NEXUS_0  saw_2  |||  cat_5  with_6  a_7  telescope_8
    *          |         /   \
    *          V        V    V
    *        we_1     a_3  white_4
    *
    * format: ON
    */
  val state1: TransitionParserState = TransitionParserState(
    stack = Vector(2, 0),
    bufferPosition = 5,
    breadcrumb = Map(0 -> -1, 1 -> 2, 2 -> 0, 3 -> 5, 4 -> 5),
    children = Map(0 -> Set(2), 2 -> Set(1), 5 -> Set(3, 4)),
    arcLabels = Map(Set(0, 2) -> SingleSymbolArcLabel('root),
      Set(2, 1) -> SingleSymbolArcLabel('nsubj), Set(3, 5) -> SingleSymbolArcLabel('det),
      Set(4, 5) -> SingleSymbolArcLabel('amod)),
    sentence =
      Sentence(Vector(NexusToken, Token.create("we", finePos = Some("NN")),
        Token.create("saw", finePos = Some("VBD")),
        Token.create("a", finePos = Some("DET")),
        Token.create("white", finePos = Some("JJ")), Token.create("cat", finePos = Some("NN")),
        Token.create("with", finePos = Some("IN")),
        Token.create("a", finePos = Some("DT")), Token.create("telescope", finePos = Some("NN"))))
  )

  /** Bogus state to test IsBracketedTransform
    * sentence:
    * "with the help of animals (insects and birds) flowers can be pollinated (fertilized)."
    */
  val state2: TransitionParserState = TransitionParserState(
    stack = Vector(0, 1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18),
    bufferPosition = 0,
    breadcrumb = Map.empty,
    children = Map.empty,
    arcLabels = Map.empty,
    sentence =
      Sentence(Vector(NexusToken, Token.create("with", finePos = Some("IN")),
        Token.create("the", finePos = Some("DT")),
        Token.create("help", finePos = Some("NN")),
        Token.create("of", finePos = Some("IN")), Token.create("animals", finePos = Some("NNS")),
        Token.create("(", finePos = Some("-LRB-")),
        Token.create("insects", finePos = Some("NNS")),
        Token.create("and", finePos = Some("CC")),
        Token.create("birds", finePos = Some("NNS")),
        Token.create(")", finePos = Some("-RRB-")), Token.create("flowers", finePos = Some("NNS")),
        Token.create("can", finePos = Some("MD")),
        Token.create("be", finePos = Some("VB")), Token.create("pollinated", finePos = Some("VBN")),
        Token.create("(", finePos = Some("-LRB-")),
        Token.create("fertilized", finePos = Some("VBN")),
        Token.create(")", finePos = Some("-RRB-")),
        Token.create(".", finePos = Some("."))))
  )

  /** Bogus state to test IsBracketedTransform
    * sentence:
    * "with the help of animals ((insects) and birds) flowers can be pollinated."
    */
  val state3: TransitionParserState = TransitionParserState(
    stack = Vector(5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    bufferPosition = 0,
    breadcrumb = Map.empty,
    children = Map.empty,
    arcLabels = Map.empty,
    sentence =
      Sentence(Vector(NexusToken, Token.create("with", finePos = Some("IN")),
        Token.create("the", finePos = Some("DT")),
        Token.create("help", finePos = Some("NN")),
        Token.create("of", finePos = Some("IN")), Token.create("animals", finePos = Some("NNS")),
        Token.create("(", finePos = Some("-LRB-")),
        Token.create("(", finePos = Some("-LRB-")),
        Token.create("insects", finePos = Some("NNS")),
        Token.create(")", finePos = Some("-RRB-")),
        Token.create("and", finePos = Some("CC")),
        Token.create("birds", finePos = Some("NNS")), Token.create(")", finePos = Some("-RRB-")),
        Token.create("flowers", finePos = Some("NNS")), Token.create("can", finePos = Some("MD")),
        Token.create("be", finePos = Some("VB")),
        Token.create("pollinated", finePos = Some("VBN"))))
  )

  /** Bogus state to test IsBracketedTransform
    * sentence:
    * "with the help of animals insects and birds) flowers can be pollinated (fertilized."
    */
  val state4: TransitionParserState = TransitionParserState(
    stack = Vector(0, 1, 7, 8, 9, 10, 12, 13, 14, 15, 16),
    bufferPosition = 0,
    breadcrumb = Map.empty,
    children = Map.empty,
    arcLabels = Map.empty,
    sentence =
      Sentence(Vector(NexusToken, Token.create("with", finePos = Some("IN")),
        Token.create("the", finePos = Some("DT")),
        Token.create("help", finePos = Some("NN")),
        Token.create("of", finePos = Some("IN")), Token.create("animals", finePos = Some("NNS")),
        Token.create("insects", finePos = Some("NNS")),
        Token.create("and", finePos = Some("CC")),
        Token.create("birds", finePos = Some("NNS")), Token.create(")", finePos = Some("-RRB-")),
        Token.create("flowers", finePos = Some("NNS")),
        Token.create("can", finePos = Some("MD")), Token.create("be", finePos = Some("VB")),
        Token.create("pollinated", finePos = Some("VBN")),
        Token.create("(", finePos = Some("-LRB-")),
        Token.create("fertilized", finePos = Some("VBN")),
        Token.create(".", finePos = Some("."))))
  )
}
/*
class TokenTransformSpec extends UnitSpec {
  // scalastyle:off

  "Calling WordTransform's apply" should "give back the stack top's word" in {
    WordTransform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('saw)
  }

  it should "give back the empty set for an invalid stack position" in {
    WordTransform(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  "Serializing a WordTransform" should "preserve a WordTransform" in {
    val transform: TokenTransform = WordTransform
    transform.toJson.convertTo[TokenTransform] shouldBe WordTransform
  }

  "Calling TokenExistenceTransform's apply" should "give back nothing when the token exists" in {
    TokenExistenceTransform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set()
  }

  it should "give back 'no for an invalid position" in {
    TokenExistenceTransform(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set('no)
  }

  "Serializing a TokenExistenceTransform" should "preserve a TokenExistenceTransform" in {
    val transform: TokenTransform = TokenExistenceTransform
    transform.toJson.convertTo[TokenTransform] shouldBe TokenExistenceTransform
  }

  "Calling KeywordTransform's apply" should "give back the stack top's word" in {
    val vocab: Set[Symbol] = Set('we, 'saw, 'a, 'white, 'cat, 'with, 'a, 'telescope)
    val transform: TokenTransform = new KeywordTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('saw)
  }

  it should "give back no features when the word is not a keyword" in {
    val vocab: Set[Symbol] = Set('we, 'a, 'white, 'cat, 'with, 'a, 'telescope)
    val transform: TokenTransform = new KeywordTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set()
  }

  it should "give back the empty set for an invalid stack position" in {
    val vocab: Set[Symbol] = Set('we, 'saw, 'a, 'white, 'cat, 'with, 'a, 'telescope)
    val transform = new KeywordTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  "Serializing a KeywordTransform" should "preserve it" in {
    val vocab: Set[Symbol] = Set('we, 'saw, 'a, 'white, 'cat, 'with, 'a, 'telescope)
    val transform: TokenTransform = new KeywordTransform(vocab)
    transform.toJson.convertTo[TokenTransform] shouldBe KeywordTransform(vocab)
  }

  "Calling TokenPropertyTransform's apply" should "give back the correct answer for the front buffer item" in {
    val transform: TokenTransform = new TokenPropertyTransform('cpos)
    transform(TokenTransformTestData.state1, BufferRef(0)) shouldBe
      Set('NN)
  }

  it should "give back the empty set for an invalid position" in {
    val transform: TokenTransform = new TokenPropertyTransform('cpos)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe Set()
  }

  "Serializing a TokenPropertyTransform" should "preserve a TokenPropertyTransform" in {
    val transform: TokenTransform = TokenPropertyTransform('cpos)
    transform.toJson.convertTo[TokenTransform] shouldBe TokenPropertyTransform('cpos)
  }

  "Calling BreadcrumbAssigned's apply" should "give back the empty set for an invalid position" in {
    BreadcrumbAssigned(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  it should "give back no for a token with no breadcrumb" in {
    BreadcrumbAssigned(TokenTransformTestData.state1, BufferRef(0)) shouldBe Set('no)
  }

  it should "give back yes for a token with an assigned breadcrumb" in {
    BreadcrumbAssigned(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('yes)
  }

  "Serializing a BreadcrumbAssigned" should "preserve a BreadcrumbAssigned" in {
    val transform: TokenTransform = BreadcrumbAssigned
    transform.toJson.convertTo[TokenTransform] shouldBe BreadcrumbAssigned
  }
/*
  "Calling ChildLabels's apply" should "give back multiple labels for the front buffer item" in {
    ChildLabels(TokenTransformTestData.state1, BufferRef(0)) shouldBe Set('det, 'amod)
  }

  it should "give back a singleton set for the top stack item" in {
    ChildLabels(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('nsubj)
  }

  it should "give back an empty set for a token without children" in {
    ChildLabels(TokenTransformTestData.state1, BufferRef(1)) shouldBe Set()
  }

  it should "give back an empty set for an invalid token position" in {
    ChildLabels(TokenTransformTestData.state1, StackRef(2)) shouldBe Set()
  }


  "Serializing a ChildLabels" should "preserve a ChildLabels" in {
    val transform: TokenTransform = ChildLabels
    transform.toJson.convertTo[TokenTransform] shouldBe ChildLabels
  }
  */

  "Calling NumChildrenToTheLeft's apply" should "correctly process the front buffer item" in {
    NumChildrenToTheLeft(5)(TokenTransformTestData.state1, BufferRef(0)) shouldBe Set(Symbol("2"))
  }

  it should "place an upper bound when requested" in {
    NumChildrenToTheLeft(1)(TokenTransformTestData.state1, BufferRef(0)) shouldBe Set(Symbol("1"))
  }

  it should "identify zero children correctly" in {
    NumChildrenToTheLeft(5)(TokenTransformTestData.state1, BufferRef(1)) shouldBe Set(Symbol("0"))
  }

  it should "give back the empty set for an invalid position" in {
    NumChildrenToTheLeft(5)(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  "Serializing a NumChildrenToTheLeft" should "preserve a NumChildrenToTheLeft(10)" in {
    val transform: TokenTransform = NumChildrenToTheLeft(10)
    transform.toJson.convertTo[TokenTransform] shouldBe NumChildrenToTheLeft(10)
  }

  "Calling SuffixTransform's apply" should "give back the stack top's matching suffixes" in {
    val vocab: Set[Symbol] = Set('ing, 'w, 'es, 'aw)
    val transform: TokenTransform = new SuffixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('w, 'aw)
  }

  it should "give back no features when the word has no matching suffixes" in {
    val vocab: Set[Symbol] = Set('ing, 'es, 'er)
    val transform: TokenTransform = new SuffixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set()
  }

  it should "give back the empty set for an invalid stack position" in {
    val vocab: Set[Symbol] = Set('ing, 'w, 'es, 'aw)
    val transform = new SuffixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  "Serializing a SuffixTransform" should "preserve it" in {
    val vocab: Set[Symbol] = Set('ing, 'es, 'er)
    val transform: TokenTransform = new SuffixTransform(vocab)
    transform.toJson.convertTo[TokenTransform] shouldBe SuffixTransform(vocab)
  }

  "Calling PrefixTransform's apply" should "give back the stack top's matching prefixes" in {
    val vocab: Set[Symbol] = Set('anti, 's, 'un, 'saw)
    val transform: TokenTransform = new PrefixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set('s, 'saw)
  }

  it should "give back no features when the word has no matching prefixes" in {
    val vocab: Set[Symbol] = Set('anti, 'so, 'un)
    val transform: TokenTransform = new PrefixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(0)) shouldBe Set()
  }

  it should "give back the empty set for an invalid stack position" in {
    val vocab: Set[Symbol] = Set('anti, 's, 'un, 'saw)
    val transform = new PrefixTransform(vocab)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe
      Set()
  }

  "Serializing a PrefixTransform" should "preserve it" in {
    val vocab: Set[Symbol] = Set('anti, 's, 'un, 'saw)
    val transform: TokenTransform = new PrefixTransform(vocab)
    transform.toJson.convertTo[TokenTransform] shouldBe PrefixTransform(vocab)
  }

  /*
  "Calling LookAheadTransform's apply" should "give back multiple labels for the front buffer item" in {
    val transform: TokenTransform = new LookAheadTransform('lcase)
    transform(TokenTransformTestData.state1, BufferRef(0)) shouldBe Set('with, 'a, 'telescope)
  }

  it should "give back an empty set for an invalid token position" in {
    val transform: TokenTransform = new LookAheadTransform('lcase)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe Set(TokenTransform.noTokenHere)
  }

  "Serializing a LookAheadTransform" should "preserve a LookAheadTransform" in {
    val transform: TokenTransform = new LookAheadTransform('lcase)
    transform.toJson.convertTo[TokenTransform] shouldBe LookAheadTransform('lcase)
  }

  "Calling LookBehindTransform's apply" should "give back multiple labels for the front buffer item" in {
    val transform: TokenTransform = new LookBehindTransform('lcase)
    transform(TokenTransformTestData.state1, BufferRef(0)) shouldBe
      Set('nexus, 'we, 'saw, 'a, 'white)
  }

  it should "give back an empty set for an invalid token position" in {
    val transform: TokenTransform = new LookBehindTransform('lcase)
    transform(TokenTransformTestData.state1, StackRef(2)) shouldBe Set(TokenTransform.noTokenHere)
  }

  "Serializing a LookBehindTransform" should "preserve a LookBehindTransform" in {
    val transform: TokenTransform = new LookBehindTransform('lcase)
    transform.toJson.convertTo[TokenTransform] shouldBe LookBehindTransform('lcase)
  }
  */

  "Calling IsBracketedTransform's apply on tokens 0, 1, 4, 5, 11, 12 and 14 in state2" should
    "give back a 'no feature because the word is not in parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state2, StackRef(0)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(1)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(2)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(3)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(9)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(10)) shouldBe Set('no)
    transform(TokenTransformTestData.state2, StackRef(11)) shouldBe Set('no)
  }

  "Calling IsBracketedTransform's apply on tokens 6, 7, 8, 9, 10, 15, 16 and 17 in state2" should
    "give back '( for left parens, ') for right parens and 'yes for all other words present in between parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state2, StackRef(4)) shouldBe Set(Symbol("("))
    transform(TokenTransformTestData.state2, StackRef(5)) shouldBe Set('yes)
    transform(TokenTransformTestData.state2, StackRef(6)) shouldBe Set('yes)
    transform(TokenTransformTestData.state2, StackRef(7)) shouldBe Set('yes)
    transform(TokenTransformTestData.state2, StackRef(8)) shouldBe Set(Symbol(")"))
    transform(TokenTransformTestData.state2, StackRef(12)) shouldBe Set(Symbol("("))
    transform(TokenTransformTestData.state2, StackRef(13)) shouldBe Set('yes)
    transform(TokenTransformTestData.state2, StackRef(14)) shouldBe Set(Symbol(")"))
  }

  "Calling IsBracketedTransform's apply on tokens 5, 13 and 14 in state3" should
    "give back a 'no feature because the word is not in parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state3, StackRef(0)) shouldBe Set('no)
    transform(TokenTransformTestData.state3, StackRef(8)) shouldBe Set('no)
    transform(TokenTransformTestData.state3, StackRef(9)) shouldBe Set('no)
  }

  "Calling IsBracketedTransform's apply on tokens 6, 7, 8, 9, 10, 11, and 12 in state3 with embedded parens" should
    "give back '( for left parens, ') for right parens and 'yes for all other words present in between parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state3, StackRef(1)) shouldBe Set(Symbol("("))
    transform(TokenTransformTestData.state3, StackRef(2)) shouldBe Set(Symbol("("))
    transform(TokenTransformTestData.state3, StackRef(3)) shouldBe Set('yes)
    transform(TokenTransformTestData.state3, StackRef(4)) shouldBe Set(Symbol(")"))
    transform(TokenTransformTestData.state3, StackRef(5)) shouldBe Set('yes)
    transform(TokenTransformTestData.state3, StackRef(6)) shouldBe Set('yes)
    transform(TokenTransformTestData.state3, StackRef(7)) shouldBe Set(Symbol(")"))
  }

  "Calling IsBracketedTransform's apply on tokens 0, 1, 7, 8 and 9 in state4 with unmatched close paren" should
    "give back ') for the right paren and 'yes for all other words present in between parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state4, StackRef(0)) shouldBe Set('yes)
    transform(TokenTransformTestData.state4, StackRef(1)) shouldBe Set('yes)
    transform(TokenTransformTestData.state4, StackRef(2)) shouldBe Set('yes)
    transform(TokenTransformTestData.state4, StackRef(3)) shouldBe Set('yes)
    transform(TokenTransformTestData.state4, StackRef(4)) shouldBe Set(Symbol(")"))
  }

  "Calling IsBracketedTransform's apply on tokens 14, 15 and 16 in state4 with unmatched open paren" should
    "give back '( for the left paren and 'yes for all other words present in between parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state4, StackRef(8)) shouldBe Set(Symbol("("))
    transform(TokenTransformTestData.state4, StackRef(9)) shouldBe Set('yes)
    transform(TokenTransformTestData.state4, StackRef(10)) shouldBe Set('yes)
  }

  "Calling IsBracketedTransform's apply on tokens 10, 12 and 13 in state4" should
    "give back a 'no feature because the word is not in parens" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state4, StackRef(5)) shouldBe Set('no)
    transform(TokenTransformTestData.state4, StackRef(6)) shouldBe Set('no)
    transform(TokenTransformTestData.state4, StackRef(7)) shouldBe Set('no)
  }

  "IsBracketedTransform" should "give back the empty set for an invalid stack position" in {
    val transform: TokenTransform = IsBracketedTransform
    transform(TokenTransformTestData.state2, StackRef(16)) shouldBe
      Set()
    transform(TokenTransformTestData.state4, StackRef(11)) shouldBe
      Set()
  }

  "Serializing a IsBracketedTransform" should "preserve it" in {
    val transform: TokenTransform = IsBracketedTransform
    transform.toJson.convertTo[TokenTransform] shouldBe IsBracketedTransform
  }
}
*/
