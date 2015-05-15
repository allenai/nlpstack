package org.allenai.nlpstack.parse.poly.eval

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }
import org.allenai.nlpstack.parse.poly.polyparser.{ InMemoryPolytreeParseSource, SingleSymbolArcLabel, PolytreeParse }

class ParseBankSpec extends UnitSpec {
  // scalastyle:off


  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    *     |
    *     |       the_1--
    *     |              \
    *     |               -->cat_2
    *     \              /
    *      -----> sat_3--
    *        /
    * by_4 --
    *        \
    *         --> me_5
    *
    * format: ON
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('the, Map('cpos -> Set('DT))),
      Token('cat, Map('cpos -> Set('NN))),
      Token('sat, Map('cpos -> Set('VB))),
      Token('by, Map('cpos -> Set('IN))),
      Token('me, Map('cpos -> Set('PRP)))
    )),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('ROOT))),
        Set((2, SingleSymbolArcLabel('DET))),
        Set((1, SingleSymbolArcLabel('DET)), (3, SingleSymbolArcLabel('NSUBJ))),
        Set((0, SingleSymbolArcLabel('ROOT)), (2, SingleSymbolArcLabel('NSUBJ)),
          (4, SingleSymbolArcLabel('PREP))),
        Set((3, SingleSymbolArcLabel('PREP)), (5, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('POBJ)))
      )
  )

  /** This represents the following parse:
    * format: OFF
    *
    * NEXUS_0
    *     |
    *     V
    *    a_1
    *    / \
    *   /   \
    *  V    V
    * b_2   d_4
    *       |
    *       V
    *      c_3
    *
    * format: ON
    */
  val parse2 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('a, Map('cpos -> Set('AA))),
      Token('b, Map('cpos -> Set('BB))),
      Token('c, Map('cpos -> Set('CC))),
      Token('d, Map('cpos -> Set('DD)))
    )),
    breadcrumb = Vector(-1, 0, 1, 4, 1),
    children = Vector(Set(1), Set(2, 4), Set(), Set(), Set(3)),
    arclabels =
      Vector(
        Set((1, SingleSymbolArcLabel('n2a))),
        Set((0, SingleSymbolArcLabel('n2a)), (2, SingleSymbolArcLabel('a2b)),
          (4, SingleSymbolArcLabel('a2d))),
        Set((1, SingleSymbolArcLabel('a2b))),
        Set((4, SingleSymbolArcLabel('d2c))),
        Set((1, SingleSymbolArcLabel('a2d)), (3, SingleSymbolArcLabel('d2c)))
      )
  )

  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    *     |
    *     |       the_1--
    *     |              \
    *     |               -->cat_2
    *     \              /
    *      ------> sat_3--
    *           /
    * "by me"_4
    *
    * format: ON
    */
  val parse1b = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('the, Map('cpos -> Set('DT))),
      Token('cat, Map('cpos -> Set('NN))),
      Token('sat, Map('cpos -> Set('VB))),
      Token(Symbol("by me"), Map('cpos -> Set('IN)))
    )),
    breadcrumb = Vector(-1, 2, 3, 0, 3),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3)),
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('ROOT))),
        Set((2, SingleSymbolArcLabel('DET))),
        Set((1, SingleSymbolArcLabel('DET)), (3, SingleSymbolArcLabel('NSUBJ))),
        Set((0, SingleSymbolArcLabel('ROOT)), (2, SingleSymbolArcLabel('NSUBJ)),
          (4, SingleSymbolArcLabel('PREP))),
        Set((3, SingleSymbolArcLabel('PREP)))
     )
  )



  "Calling ParseBank.askForGoldParse" should "return the requested parse" in {
    val bank = ParseBank.createParseBankFromSource(
      InMemoryPolytreeParseSource(Seq(parse1, parse2))
    )
    bank.askForGoldParse(parse1) shouldBe Some(parse1)
  }

  it should "return None if the requested parse is missing" in {
    val bank = ParseBank.createParseBankFromSource(
      InMemoryPolytreeParseSource(Seq(parse2))
    )
    bank.askForGoldParse(parse1) shouldBe None
  }

  it should "return None if the requested parse has a different tokenization" in {
    val bank = ParseBank.createParseBankFromSource(
      InMemoryPolytreeParseSource(Seq(parse1, parse2))
    )
    bank.askForGoldParse(parse1b) shouldBe None
  }

  "Calling ParseBank.createParseBankFromSource" should "prefer later parses" in {
    val bank = ParseBank.createParseBankFromSource(
      InMemoryPolytreeParseSource(Seq(parse1, parse2, parse1b))
    )
    bank.askForGoldParse(parse1b) shouldBe Some(parse1b)
    bank.askForGoldParse(parse1) shouldBe None
  }

}
