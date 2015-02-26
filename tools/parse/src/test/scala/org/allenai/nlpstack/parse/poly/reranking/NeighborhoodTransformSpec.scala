package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }
import org.allenai.nlpstack.parse.poly.ml.FeatureName
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

class NeighborhoodTransformSpec extends UnitSpec {

  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    *   |
    *   |       the_1--
    *   |              \
    *   |               -->cat_2
    *   \              /
    *    -----> sat_3--
    *  /
    * by_4
    *  \
    *   --> me_5
    *
    * format: ON
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(NexusToken, Token('the), Token('cat), Token('sat),
      Token('by), Token('me))),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels = Vector(Set((3, 'root)), Set((2, 'det)), Set((1, 'det), (3, 'nsubj)),
      Set((0, 'root), (2, 'nsubj), (4, 'prep)), Set((3, 'prep), (5, 'pobj)), Set((4, 'pobj))))

  "Calling the .apply method of SuffixNhTransform" should "return the correct value" in {
    val transform = SuffixNhTransform(Seq("at", "cat"))
    transform(parse1, Neighborhood(Seq(1, 2, 3))).toSet shouldBe Set(
      FeatureName(Seq('at)), FeatureName(Seq('cat))
    )
    transform(parse1, Neighborhood(Seq(1, 3))).toSet shouldBe Set(
      FeatureName(Seq('at))
    )
    transform(parse1, Neighborhood(Seq(1, 4, 5))).toSet shouldBe Set()
  }

  "Calling the .apply method of KeywordNhTransform" should "return the correct value" in {
    val transform = KeywordNhTransform(Seq("at", "cat", "BY"))
    transform(parse1, Neighborhood(Seq(1, 2, 4))).toSet shouldBe Set(
      FeatureName(Seq('cat)), FeatureName(Seq('by))
    )
    transform(parse1, Neighborhood(Seq(1, 3, 5))).toSet shouldBe Set()
  }

  "Calling the .apply method of ArclabelNhTransform" should "return the correct value" in {
    ArclabelNhTransform(parse1, Neighborhood(Seq(1, 2))) shouldBe Seq(FeatureName(Seq('det)))
    ArclabelNhTransform(parse1, Neighborhood(Seq(2, 3))) shouldBe Seq(FeatureName(Seq('nsubj)))
    ArclabelNhTransform(parse1, Neighborhood(Seq(4, 5))) shouldBe Seq(FeatureName(Seq('pobj)))
  }

  "Calling the .apply method of DirectionNhTransform" should "return the correct value" in {
    DirectionNhTransform(parse1, Neighborhood(Seq(1, 2))) shouldBe Seq(FeatureName(Seq('L)))
    DirectionNhTransform(parse1, Neighborhood(Seq(2, 1))) shouldBe Seq(FeatureName(Seq('R)))
  }

  "Calling the .apply method of CardinalityNhTransform" should "return the correct value" in {
    CardinalityNhTransform(parse1, Neighborhood(Seq(1, 4, 2))) shouldBe
      Seq(FeatureName(Seq(Symbol("3"))))
    CardinalityNhTransform(parse1, Neighborhood(Seq())) shouldBe
      Seq(FeatureName(Seq(Symbol("0"))))
  }
}
