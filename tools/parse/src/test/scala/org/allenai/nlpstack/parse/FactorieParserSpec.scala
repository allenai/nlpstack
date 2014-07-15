package org.allenai.nlpstack.parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.postag.defaultPostagger

class FactorieParserSpec extends UnitSpec {
  private def parseTreeString(text: String) = {
    val tokens = defaultTokenizer.tokenize(text)
    val postaggedTokens = defaultPostagger.postagTokenized(tokens)

    val parser = new FactorieParser
    val parseTree = parser.dependencyGraphPostagged(postaggedTokens)

    DependencyGraph.multilineStringFormat.write(parseTree)
  }

  /*
   * Tests are ignored because they take more memory than Travis is willing to
   * give them.
   *
   * When these tests fail with anything but an exception, it's a judgement call
   * whether the trees that the parser produces are valid parses or whether this
   * is a genuine error. If in doubt, consult your favorite linguist, but by and
   * large, don't worry too much about accuracy here. This is not a quality test
   * suite.
   */

  ignore should "correctly parse a simple sentence" in {
    val parseTreeStr = parseTreeString("A waffle is like a pancake with a syrup trap.")
    val expectedParseTreeStr =
      """|det(waffle-2, A-1)
         |nsubj(is-3, waffle-2)
         |root(ROOT-0, is-3)
         |prep(is-3, like-4)
         |det(pancake-6, a-5)
         |pobj(like-4, pancake-6)
         |prep(pancake-6, with-7)
         |det(trap-10, a-8)
         |nn(trap-10, syrup-9)
         |pobj(with-7, trap-10)
         |punct(is-3, .-11)""".stripMargin
    assert(parseTreeStr === expectedParseTreeStr)
  }

  ignore should "correctly parse a complicated sentence" in {
    // This sentence has two roots when it comes out of Factorie.
    val parseTreeStr = parseTreeString("Big investment banks refused to step up to the plate, traders say.")
    val expectedParseTreeStr =
      """|amod(banks-3, Big-1)
         |nn(banks-3, investment-2)
         |nsubj(refused-4, banks-3)
         |root(ROOT-0, refused-4)
         |aux(step-6, to-5)
         |xcomp(refused-4, step-6)
         |prt(step-6, up-7)
         |prep(step-6, to-8)
         |det(plate-10, the-9)
         |pobj(to-8, plate-10)
         |punct(say-13, ,-11)
         |nsubj(say-13, traders-12)
         |punct(say-13, .-14)""".stripMargin
    assert(parseTreeStr === expectedParseTreeStr)
  }
}
