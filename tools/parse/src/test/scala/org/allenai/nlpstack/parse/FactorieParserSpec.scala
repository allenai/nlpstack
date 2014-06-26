package org.allenai.nlpstack.parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.graph.DependencyGraph
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.postag.defaultPostagger

class FactorieParserSpec extends UnitSpec {
  // Disabled due to memory limitations in Travis. Should run fine locally.

  /*
  "FactorieParser" should "correctly parse a simple sentence" in {
    val text = "A waffle is like a pancake with a syrup trap."

    val tokens = defaultTokenizer.tokenize(text)
    val postaggedTokens = defaultPostagger.postagTokenized(tokens)

    val parser = new FactorieParser
    val parseTree = parser.dependencyGraphPostagged(postaggedTokens)

    val parseTreeStr = DependencyGraph.multilineStringFormat.write(parseTree)
    val expectedParseTreeStr =
      """|ParseTreeLabel(det)(waffle-2, A-1)
        |ParseTreeLabel(nsubj)(is-3, waffle-2)
        |root(ROOT-0, is-3)
        |ParseTreeLabel(prep)(is-3, like-4)
        |ParseTreeLabel(det)(pancake-6, a-5)
        |ParseTreeLabel(pobj)(like-4, pancake-6)
        |ParseTreeLabel(prep)(pancake-6, with-7)
        |ParseTreeLabel(det)(trap-10, a-8)
        |ParseTreeLabel(nn)(trap-10, syrup-9)
        |ParseTreeLabel(pobj)(with-7, trap-10)
        |ParseTreeLabel(punct)(is-3, .-11)""".stripMargin
    assert(parseTreeStr === expectedParseTreeStr)
  }
  */
}
