package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }
import org.allenai.nlpstack.parse.poly.polyparser.{ Neighborhood, PolytreeParse }

class NeighborhoodExtractorSpec extends UnitSpec {

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
    *   /
    * by_4
    *   \
    *    --> me_5
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


  "Calling the .apply method of AllChildrenExtractor" should "return the correct value" in {
    AllChildrenExtractor(parse1, 0) shouldBe Seq(Neighborhood(Seq(3)))
    AllChildrenExtractor(parse1, 1) shouldBe Seq(Neighborhood(Seq(2)))
    AllChildrenExtractor(parse1, 2) shouldBe Seq(Neighborhood(Seq()))
    AllChildrenExtractor(parse1, 3) shouldBe Seq(Neighborhood(Seq(2)))
    AllChildrenExtractor(parse1, 4) shouldBe Seq(Neighborhood(Seq(3, 5)))
    AllChildrenExtractor(parse1, 5) shouldBe Seq(Neighborhood(Seq()))
  }

  "Calling the .apply method of AllParentsExtractor" should "return the correct value" in {
    AllParentsExtractor(parse1, 0) shouldBe Seq(Neighborhood(Seq()))
    AllParentsExtractor(parse1, 1) shouldBe Seq(Neighborhood(Seq()))
    AllParentsExtractor(parse1, 2) shouldBe Seq(Neighborhood(Seq(1, 3)))
    AllParentsExtractor(parse1, 3) shouldBe Seq(Neighborhood(Seq(0, 4)))
    AllParentsExtractor(parse1, 4) shouldBe Seq(Neighborhood(Seq()))
    AllParentsExtractor(parse1, 5) shouldBe Seq(Neighborhood(Seq(4)))
    AllParentsExtractor(parse1, 5) shouldBe Seq(Neighborhood(Seq(4)))
  }

  "Calling the .apply method of EachChildExtractor" should "return the correct value" in {
    EachChildExtractor(parse1, 0) shouldBe Seq(
      Neighborhood(Seq(3))
    )
    EachChildExtractor(parse1, 1) shouldBe Seq(
      Neighborhood(Seq(2))
    )
    EachChildExtractor(parse1, 2) shouldBe Seq()
    EachChildExtractor(parse1, 3) shouldBe Seq(
      Neighborhood(Seq(2))
    )
    EachChildExtractor(parse1, 4).toSet shouldBe Set(
      Neighborhood(Seq(3)),
      Neighborhood(Seq(5))
    )
    EachChildExtractor(parse1, 5) shouldBe Seq()
  }

  "Calling the .apply method of EachParentExtractor" should "return the correct value" in {
    EachParentExtractor(parse1, 0) shouldBe Seq()
    EachParentExtractor(parse1, 1) shouldBe Seq()
    EachParentExtractor(parse1, 2).toSet shouldBe Set(
      Neighborhood(Seq(1)),
      Neighborhood(Seq(3))
    )
    EachParentExtractor(parse1, 3).toSet shouldBe Set(
      Neighborhood(Seq(0)),
      Neighborhood(Seq(4))
    )
    EachParentExtractor(parse1, 4) shouldBe Seq()
    EachParentExtractor(parse1, 5) shouldBe Seq(
      Neighborhood(Seq(4))
    )
  }

  "Calling the .apply method of SingleChildExtractor" should "return the correct value" in {
    SpecificChildExtractor(0)(parse1, 4) shouldBe Seq(
      Neighborhood(Seq(3))
    )
    SpecificChildExtractor(1)(parse1, 4) shouldBe Seq(
      Neighborhood(Seq(5))
    )
    SpecificChildExtractor(2)(parse1, 4) shouldBe Seq()
    SpecificChildExtractor(0)(parse1, 5) shouldBe Seq()
  }

  "Calling the .apply method of SingleParentExtractor" should "return the correct value" in {
    SpecificParentExtractor(0)(parse1, 3) shouldBe Seq(
      Neighborhood(Seq(0))
    )
    SpecificParentExtractor(1)(parse1, 3) shouldBe Seq(
      Neighborhood(Seq(4))
    )
    SpecificParentExtractor(2)(parse1, 3) shouldBe Seq()
    SpecificParentExtractor(0)(parse1, 0) shouldBe Seq()
    SpecificParentExtractor(0)(parse1, 1) shouldBe Seq()
  }

  "Calling the .apply method of SelfAndSpecificChildExtractor" should "return the correct value" in {
    SelfAndSpecificChildExtractor(0)(parse1, 4) shouldBe Seq(
      Neighborhood(Seq(3, 4))
    )
    SelfAndSpecificChildExtractor(1)(parse1, 4) shouldBe Seq(
      Neighborhood(Seq(5, 4))
    )
    SelfAndSpecificChildExtractor(2)(parse1, 4) shouldBe Seq()
    SelfAndSpecificChildExtractor(0)(parse1, 5) shouldBe Seq()
  }

  "Calling the .apply method of SelfAndSpecificParentExtractor" should "return the correct value" in {
    SelfAndSpecificParentExtractor(0)(parse1, 3) shouldBe Seq(
      Neighborhood(Seq(0, 3))
    )
    SelfAndSpecificParentExtractor(1)(parse1, 3) shouldBe Seq(
      Neighborhood(Seq(4, 3))
    )
    SelfAndSpecificParentExtractor(2)(parse1, 3) shouldBe Seq()
    SelfAndSpecificParentExtractor(0)(parse1, 0) shouldBe Seq()
    SelfAndSpecificParentExtractor(0)(parse1, 1) shouldBe Seq()
  }

  "Calling the .apply method of SelfExtractor" should "return the correct value" in {
    SelfExtractor(parse1, 0) shouldBe Seq(Neighborhood(Seq(0)))
    SelfExtractor(parse1, 1) shouldBe Seq(Neighborhood(Seq(1)))
    SelfExtractor(parse1, 2) shouldBe Seq(Neighborhood(Seq(2)))
    SelfExtractor(parse1, 3) shouldBe Seq(Neighborhood(Seq(3)))
    SelfExtractor(parse1, 4) shouldBe Seq(Neighborhood(Seq(4)))
    SelfExtractor(parse1, 5) shouldBe Seq(Neighborhood(Seq(5)))
  }

  "Calling the .apply method of ParseNeighborhoodExtractor" should "return the correct value" in {
    val extractor = new ParseNeighborhoodExtractor(EachParentExtractor)
    extractor(parse1).toSet shouldBe Set(
      Neighborhood(Seq(1)),
      Neighborhood(Seq(3)),
      Neighborhood(Seq(0)),
      Neighborhood(Seq(4))
    )
  }
}
