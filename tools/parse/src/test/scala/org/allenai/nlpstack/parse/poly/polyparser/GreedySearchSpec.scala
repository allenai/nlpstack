package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.GreedySearch

class GreedySearchSpec extends UnitSpec {
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
    arclabels = Vector(Set((3, 'ROOT)), Set((2, 'DET)), Set((1, 'DET), (3, 'NSUBJ)),
      Set((0, 'ROOT), (2, 'NSUBJ), (4, 'PREP)), Set((3, 'PREP), (5, 'POBJ)), Set((4, 'POBJ)))
  )

  "Calling GreedySearch's find" should "re-create parse1" in {
    val transitionSystem = ArcEagerTransitionSystem(parse1.sentence, Set(), Seq())
    val costFunction = new ArcEagerGuidedCostFunction(parse1, transitionSystem)
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(
      costFunction.transitionSystem.initialState(Seq()).get,
      constraints = Set()
    )
    bestWalk map { _.transitions } shouldBe Some(List(
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc('DET),
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc('NSUBJ),
      ArcEagerRightArc(),
      LabelRightArc('ROOT),
      ArcEagerRightArc(),
      LabelRightArc('PREP),
      ArcEagerRightArc(),
      LabelRightArc('POBJ),
      ArcEagerReduce,
      ArcEagerReduce,
      ArcEagerReduce
    ))
  }

  it should "have a failure if we create an impossible constraint" in {
    val transitionSystem = ArcEagerTransitionSystem(parse1.sentence, Set(), Seq())
    val costFunction = new ArcEagerGuidedCostFunction(parse1, transitionSystem)
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(
      costFunction.transitionSystem.initialState(Seq()).get,
      constraints = Set(ForbiddenEdge(3, 4))
    )
    bestWalk shouldBe None
  }

  it should "recreate parse1 if we only forbid unnecessary arcs" in {
    val transitionSystem = ArcEagerTransitionSystem(parse1.sentence, Set(), Seq())
    val costFunction = new ArcEagerGuidedCostFunction(parse1, transitionSystem)
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(
      costFunction.transitionSystem.initialState(Seq()).get,
      constraints = Set(ForbiddenEdge(3, 5), ForbiddenEdge(1, 3))
    )
    bestWalk map { _.transitions } shouldBe Some(List(
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc('DET),
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc('NSUBJ),
      ArcEagerRightArc(),
      LabelRightArc('ROOT),
      ArcEagerRightArc(),
      LabelRightArc('PREP),
      ArcEagerRightArc(),
      LabelRightArc('POBJ),
      ArcEagerReduce,
      ArcEagerReduce,
      ArcEagerReduce
    ))
  }


  //TODO: some test that exercises a cost function that returns scores for multiple transitions
}
