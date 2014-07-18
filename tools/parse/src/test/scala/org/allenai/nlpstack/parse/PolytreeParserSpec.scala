package org.allenai.nlpstack.parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

class PolytreeParserSpec extends UnitSpec {
  private def parseTreeString(text: String) = {
    val tokens = defaultTokenizer.tokenize(text)
    val postaggedTokens = defaultPostagger.postagTokenized(tokens)

    val parser = new PolytreeParser
    val parseTree = parser.dependencyGraphPostagged(postaggedTokens)

    DependencyGraph.multilineStringFormat.write(parseTree)
  }

  /*
   * When these tests fail with anything but an exception, it's a judgement call
   * whether the trees that the parser produces are valid parses or whether this
   * is a genuine error. If in doubt, consult your favorite linguist, but by and
   * large, don't worry too much about accuracy here. This is not a quality test
   * suite.
   */

  "PolytreeParserParser" should "correctly parse a simple sentence" in {
    val parseTreeStr = parseTreeString("A waffle is like a pancake with a syrup trap.")
    val expectedParseTreeStr =
      """|DET(waffle-2, A-1)
         |NSUBJ(is-3, waffle-2)
         |root(ROOT-0, is-3)
         |PREP(is-3, like-4)
         |DET(pancake-6, a-5)
         |POBJ(like-4, pancake-6)
         |PREP(is-3, with-7)
         |DET(trap-10, a-8)
         |NN(trap-10, syrup-9)
         |POBJ(with-7, trap-10)
         |PUNCT(is-3, .-11)""".stripMargin
    assert(parseTreeStr === expectedParseTreeStr)
  }

  it should "correctly parse a complicated sentence" in {
    // This sentence has two roots when it comes out of Factorie, so we want to
    // test the same case here.
    val parseTreeStr = parseTreeString("Big investment banks refused to step up to the plate, traders say.")
    val expectedParseTreeStr =
      """|AMOD(banks-3, Big-1)
         |NN(banks-3, investment-2)
         |NSUBJ(refused-4, banks-3)
         |root(ROOT-0, refused-4)
         |AUX(step-6, to-5)
         |XCOMP(refused-4, step-6)
         |PRT(step-6, up-7)
         |PREP(step-6, to-8)
         |DET(plate-10, the-9)
         |POBJ(to-8, plate-10)
         |PUNCT(say-13, ,-11)
         |NSUBJ(say-13, traders-12)
         |DEP(refused-4, say-13)
         |PUNCT(refused-4, .-14)""".stripMargin
    assert(parseTreeStr === expectedParseTreeStr)
  }
}
