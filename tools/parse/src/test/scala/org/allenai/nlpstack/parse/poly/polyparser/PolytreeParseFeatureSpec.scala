package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector => MLFeatureVector, FeatureName => MLFeatureName }
import org.allenai.nlpstack.parse.poly.reranking.BaseParserScoreFeature

class PolytreeParseFeatureSpec extends UnitSpec {
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
    sentence = Sentence(Vector(NexusToken, Token('the), Token('cat), Token('sat),
      Token('by), Token('me))),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('root))),
        Set((2, SingleSymbolArcLabel('det))),
        Set((1, SingleSymbolArcLabel('det)), (3, SingleSymbolArcLabel('nsubj))),
        Set((0, SingleSymbolArcLabel('root)), (2, SingleSymbolArcLabel('nsubj)),
          (4, SingleSymbolArcLabel('prep))),
        Set((3, SingleSymbolArcLabel('prep)), (5, SingleSymbolArcLabel('pobj))),
        Set((4, SingleSymbolArcLabel('pobj)))
      )
  )

  "Calling the .apply method of BaseParserScoreFeature" should "return the correct value" in {
    val featureName = BaseParserScoreFeature.name
    BaseParserScoreFeature(parse1, 12.0) shouldBe MLFeatureVector(Seq(
      MLFeatureName(List(featureName)) -> 12.0
    ))
  }
}
