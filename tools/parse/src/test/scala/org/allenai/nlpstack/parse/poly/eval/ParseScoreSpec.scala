package org.allenai.nlpstack.parse.poly.eval

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }
import org.allenai.nlpstack.parse.poly.polyparser.{ InMemoryPolytreeParseSource, SingleSymbolArcLabel, PolytreeParse }

class ParseScoreSpec extends UnitSpec {
  // scalastyle:off


  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    * |
    * |                -->he_1
    * \               /
    * -----> ate_2 --
    * /          \
    * |            -->pasta_3
    * |
    * with_4 --
    * \
    * -->meatballs_5
    *
    * format: ON
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('he, Map('cpos -> Set('NN))),
      Token('ate, Map('cpos -> Set('VB))),
      Token('pasta, Map('cpos -> Set('NN))),
      Token('with, Map('cpos -> Set('IN))),
      Token('meatballs, Map('cpos -> Set('NNS)))
    )),
    breadcrumb = Vector(-1, 2, 0, 2, 2, 4),
    children = Vector(Set(2), Set(), Set(1, 3), Set(), Set(2, 5), Set()),
    arclabels =
      Vector(
        Set((2, SingleSymbolArcLabel('CONJ))),
        Set((2, SingleSymbolArcLabel('NSUBJ))),
        Set((1, SingleSymbolArcLabel('NSUBJ)), (3, SingleSymbolArcLabel('DOBJ)),
          (4, SingleSymbolArcLabel('PREP)), (0, SingleSymbolArcLabel('CONJ))),
        Set((2, SingleSymbolArcLabel('DOBJ))),
        Set((2, SingleSymbolArcLabel('PREP)), (5, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('POBJ)))
      )
  )

  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    * |
    * |                -->he_1
    * \               /
    * -----> ate_2 --
    * \
    * -->pasta_3
    * /
    * with_4 --
    * \
    * -->meatballs_5
    *
    * format: ON
    */
  val parse2 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('he, Map('cpos -> Set('NN))),
      Token('ate, Map('cpos -> Set('VB))),
      Token('pasta, Map('cpos -> Set('NN))),
      Token('with, Map('cpos -> Set('IN))),
      Token('meatballs, Map('cpos -> Set('NN)))
    )),
    breadcrumb = Vector(-1, 2, 0, 2, 3, 4),
    children = Vector(Set(2), Set(), Set(1, 3), Set(), Set(3, 5), Set()),
    arclabels =
      Vector(
        Set((2, SingleSymbolArcLabel('ROOT))),
        Set((2, SingleSymbolArcLabel('NSUBJ))),
        Set((1, SingleSymbolArcLabel('NSUBJ)), (3, SingleSymbolArcLabel('DOBJ)),
          (0, SingleSymbolArcLabel('ROOT))),
        Set((2, SingleSymbolArcLabel('DOBJ)), (4, SingleSymbolArcLabel('PREP))),
        Set((3, SingleSymbolArcLabel('PREP)), (5, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('POBJ)))
      )
  )

  "Calling PathAccuracyScore.findEarliestPathDifference" should "return the correct answer" in {
    PathAccuracyScore.findEarliestPathDifference(
      5, parse1, parse2, ignorePathLabels = true, useCrumbOnly = false
    ) shouldBe Some((4, 3))
    PathAccuracyScore.findEarliestPathDifference(
      1, parse1, parse2, ignorePathLabels = true, useCrumbOnly = false
    ) shouldBe None
    PathAccuracyScore.findEarliestPathDifference(
      5, parse1, parse2, ignorePathLabels = true, useCrumbOnly = true
    ) shouldBe None
    PathAccuracyScore.findEarliestPathDifference(
      4, parse1, parse2, ignorePathLabels = true, useCrumbOnly = true
    ) shouldBe Some((2, 3))
    PathAccuracyScore.findEarliestPathDifference(
      5, parse1, parse2, ignorePathLabels = false, useCrumbOnly = false
    ) shouldBe Some((2, 2))
  }

  "UnlabeledPathAccuracy.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = UnlabeledPathAccuracy(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(3, 5)
  }

  "LabeledPathAccuracy.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = LabeledPathAccuracy(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(0, 5)
  }

  "UnlabeledAttachmentScore.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = UnlabeledAttachmentScore(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(4, 5)
  }

  "LabeledAttachmentScore.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = LabeledAttachmentScore(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(3, 5)
  }

  "UnlabeledLostTokens.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = UnlabeledLostTokens(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(2, 5)
  }

  "LabeledLostTokens.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = LabeledLostTokens(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(5, 5)
  }

  "PostagAccuracy.getRatio" should "return the correct answer" in {
    val goldBank = ParseBank.createParseBankFromSource(InMemoryPolytreeParseSource(Seq(parse2)))
    val score = PostagAccuracy(goldBank)
    score.getRatio(parse1) shouldBe Tuple2(4, 5)
  }
}
