package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.{ GreedySearch, StateCostFunction, State }

class ArcEagerTransitionSystemSpec extends UnitSpec {
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

  "Calling GuidedCostFunction's apply" should "duplicate the steps needed to create parse1" in {
    val transitionSystem = ArcEagerTransitionSystem(parse1.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions } shouldBe Some(List(
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('DET, 'DT)),
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('NSUBJ, 'NN)),
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('ROOT, 'VB)),
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('PREP, 'IN)),
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('POBJ, 'PRP)),
      ArcEagerReduce,
      ArcEagerReduce,
      ArcEagerReduce
    ))
  }

  it should "duplicate the steps needed to create parse2" in {
    val transitionSystem = ArcEagerTransitionSystem(parse2.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions} shouldBe Some(List(
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('n2a, 'AA)),
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('a2b, 'BB)),
      ArcEagerShift,
      ArcEagerLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('d2c, 'CC)),
      ArcEagerReduce,
      ArcEagerRightArc(),
      LabelRightArc(DependencyParsingArcLabel('a2d, 'DD)),
      ArcEagerReduce,
      ArcEagerReduce
    ))
  }
}
