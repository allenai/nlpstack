package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.TransitionSystemFactory

class ArcEagerConstraintsSpec extends UnitSpec {
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
    arcLabels =
      Map(
        Set(0, 2) -> DependencyParsingArcLabel('root, 'nexus),
        Set(2, 1) -> DependencyParsingArcLabel('nsubj, 'nn),
        Set(3, 5) -> DependencyParsingArcLabel('det, 'dt),
        Set(4, 5) -> DependencyParsingArcLabel('amod, 'jj)
      ),
    sentence = Sentence(Vector(NexusToken, Token('we), Token('saw), Token('a),
        Token('white), Token('cat), Token('with), Token('a), Token('telescope)))
  )

  val arcEagerFactory: TransitionSystemFactory = new ArcEagerTransitionSystemFactory(Seq())
  val arcEagerSystem = arcEagerFactory.buildTransitionSystem(state1.sentence, Set())

  "Calling ForbiddenEdge's .apply" should "return false for a Shift transition" in {
    val constraint = ForbiddenEdge(2, 5)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerShift) shouldBe false
  }

  it should "return false for a Reduce transition" in {
    val constraint = ForbiddenEdge(2, 5)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerReduce) shouldBe false
  }

  it should "return true for any other transition" in {
    val constraint = ForbiddenEdge(2, 5)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe true
    interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe true
  }

  it should "return false for a LeftArc if the stack top and buffer top do not match the " +
    "constraint" in {
      val interpretation1 = arcEagerSystem.interpretConstraint(ForbiddenEdge(2, 3))
      val interpretation2 = arcEagerSystem.interpretConstraint(ForbiddenEdge(3, 5))
      interpretation1(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
      interpretation2(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
    }

  "Calling RequestedArc's .apply" should "return true if we shift node B and A and B aren't yet" +
    "neighbors" in {
      val constraint = RequestedArc(0, 5, None)
      val interpretation = arcEagerSystem.interpretConstraint(constraint)
      interpretation(state1, ArcEagerShift) shouldBe true
    }

  it should "return false if we shift node B and A and B are already neighbors" in {
    val constraint = RequestedArc(3, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerShift) shouldBe false
  }

  it should "return true if we shift node B and A is the stack top" in {
    val constraint = RequestedArc(2, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerShift) shouldBe true
  }

  it should "return true if we try to draw a right arc to node B and A and B aren't yet" +
    "neighbors" in {
      val constraint = RequestedArc(0, 5, None)
      val interpretation = arcEagerSystem.interpretConstraint(constraint)
      interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe true
    }

  it should "return false if we draw a right arc to node B and A and B are already neighbors" in {
    val constraint = RequestedArc(3, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }

  it should "return false if we draw a right arc to node B and A is the stack top" in {
    val constraint = RequestedArc(2, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }

  it should "return true if we reduce node A and A and B aren't yet " +
    "neighbors" in {
      val constraint = RequestedArc(2, 6, None)
      val interpretation = arcEagerSystem.interpretConstraint(constraint)
      interpretation(state1, ArcEagerReduce) shouldBe true
    }

  it should "return false if we reduce node A and A and B are already neighbors" in {
    val constraint = RequestedArc(1, 2, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerReduce) shouldBe false
  }

  it should "return true if we reduce node A and B is the stack top" in {
    val constraint = RequestedArc(2, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerReduce) shouldBe true
  }

  it should "return true if we try to draw a left arc to node A and A and B aren't yet" +
    "neighbors" in {
      val constraint = RequestedArc(2, 6, None)
      val interpretation = arcEagerSystem.interpretConstraint(constraint)
      interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe true
    }

  it should "return false if we draw a left arc to node A and A and B are already neighbors" in {
    val constraint = RequestedArc(2, 1, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }

  it should "return false if we draw a left arc to node A and B is the stack top" in {
    val constraint = RequestedArc(2, 5, None)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }

  it should "return false if we draw an arc between A and B with the right label" in {
    val constraint = RequestedArc(2, 5, Some('foo))
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
    interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }

  "Calling ForbiddenArcLabel's .apply" should "return false if we draw an arc between " +
    "A and B with some non-forbidden label" in {

    val constraint = ForbiddenArcLabel(2, 5, 'bar)
    val interpretation = arcEagerSystem.interpretConstraint(constraint)
    interpretation(state1, ArcEagerLeftArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
    interpretation(state1, ArcEagerRightArc(DependencyParsingArcLabel('foo, 'foo))) shouldBe false
  }
}
