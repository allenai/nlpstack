package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }
import org.allenai.nlpstack.parse.poly.fsm.{ GreedySearch, StateTransition, StateCostFunction, State }

class ArcHybridTransitionSystemSpec extends UnitSpec {
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
    *  |
    *  V
    * c_3
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
    breadcrumb = Vector(-1, 0, 1, 2, 1),
    children = Vector(Set(1), Set(2, 4), Set(3), Set(), Set()),
    arclabels = Vector(
      Set((1, SingleSymbolArcLabel('n2a))),
      Set((0, SingleSymbolArcLabel('n2a)), (2, SingleSymbolArcLabel('a2b)),
        (4, SingleSymbolArcLabel('a2d))),
      Set((1, SingleSymbolArcLabel('a2b)), (3, SingleSymbolArcLabel('b2c))),
      Set((2, SingleSymbolArcLabel('b2c))),
      Set((1, SingleSymbolArcLabel('a2d)))
    )
  )

  "Calling GuidedCostFunction's apply" should "duplicate the steps needed to create parse1" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions} shouldBe Some(List(
      ArcHybridShift,
      ArcHybridLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('DET, 'DT)),
      ArcHybridShift,
      ArcHybridLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('NSUBJ, 'NN)),
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridRightArc(),
      LabelRightArc(DependencyParsingArcLabel('POBJ, 'PRP)),
      ArcHybridRightArc(),
      LabelRightArc(DependencyParsingArcLabel('PREP, 'IN)),
      ArcHybridLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('ROOT, 'VB)),
      ArcHybridShift
    ))
  }

  it should "duplicate the steps needed to create parse2" in {
    val transitionSystem = ArcHybridTransitionSystem(parse2.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions} shouldBe Some(List(
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridRightArc(),
      LabelRightArc(DependencyParsingArcLabel('b2c, 'CC)),
      ArcHybridRightArc(),
      LabelRightArc(DependencyParsingArcLabel('a2b, 'BB)),
      ArcHybridShift,
      ArcHybridRightArc(),
      LabelRightArc(DependencyParsingArcLabel('a2d, 'DD)),
      ArcHybridLeftArc(),
      LabelLeftArc(DependencyParsingArcLabel('n2a, 'AA)),
      ArcHybridShift
    ))
  }

  it should "recreate parse1" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    val greedySearch = new GreedySearch(costFunction)
    val finalState = greedySearch.find(initialState, Set()) flatMap { walk => walk.finalState }
    val finalSculpture = finalState flatMap { state => transitionSystem.toSculpture(state) }
    finalSculpture.get.toString shouldBe parse1.toString
    finalSculpture map { sculpture => sculpture match {
      case polyparse: PolytreeParse =>
        polyparse.breadcrumb shouldBe parse1.breadcrumb
        polyparse.children shouldBe parse1.children
        polyparse.arclabels shouldBe parse1.arclabels
    }}
  }

  it should "recreate parse2" in {
    val transitionSystem = ArcHybridTransitionSystem(parse2.sentence, Set(), Seq())
    val initialState: State = transitionSystem.initialState(Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    val greedySearch = new GreedySearch(costFunction)
    val finalState = greedySearch.find(initialState, Set()) flatMap { walk => walk.finalState }
    val finalSculpture = finalState flatMap { state => transitionSystem.toSculpture(state) }
    finalSculpture.get.toString shouldBe parse2.toString
    finalSculpture map { sculpture => sculpture match {
      case polyparse: PolytreeParse =>
        polyparse.breadcrumb shouldBe parse2.breadcrumb
        polyparse.children shouldBe parse2.children
        polyparse.arclabels shouldBe parse2.arclabels
    }}
  }

  "The ForbiddenEdge's interpretation" should "return true for a left arc" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val interpretation = ArcHybridForbiddenArcInterpretation(ForbiddenEdge(2, 3))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LabelLeftArc(SingleSymbolArcLabel('DET)),
        ArcHybridShift
      )
    )
    interpretation(state.get, ArcHybridLeftArc()) shouldBe true
    interpretation(state.get, ArcHybridRightArc()) shouldBe false
  }

  it should "return true for a right arc" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val interpretation = ArcHybridForbiddenArcInterpretation(ForbiddenEdge(4, 5))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LabelLeftArc(SingleSymbolArcLabel('DET)),
        ArcHybridShift,
        ArcHybridLeftArc(),
        LabelLeftArc(SingleSymbolArcLabel('NSUBJ)),
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridShift
      )
    )
    interpretation(state.get, ArcHybridLeftArc()) shouldBe false
    interpretation(state.get, ArcHybridRightArc()) shouldBe true
  }

  "The ForbiddenArcLabel's interpretation" should "return true for a mislabeled left arc" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val interpretation1 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 2, 'DET))
    val interpretation2 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 3, 'DET))
    val interpretation3 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(0, 2, 'DET))
    val interpretation4 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 2, 'POBJ))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc()
      )
    )
    interpretation1(state.get, LabelLeftArc(DependencyParsingArcLabel('DET, 'DT))) shouldBe true
    interpretation2(state.get, LabelLeftArc(DependencyParsingArcLabel('DET, 'DT))) shouldBe false
    interpretation3(state.get, LabelLeftArc(DependencyParsingArcLabel('DET, 'DT))) shouldBe false
    interpretation4(state.get, LabelLeftArc(DependencyParsingArcLabel('DET, 'DT))) shouldBe false
  }

  it should "return true for a mislabeled right arc" in {
    val transitionSystem = ArcHybridTransitionSystem(parse1.sentence, Set(), Seq())
    val interpretation1 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 5, 'POBJ))
    val interpretation2 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 3, 'POBJ))
    val interpretation3 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(3, 5, 'POBJ))
    val interpretation4 =
      ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 5, 'DET))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LabelLeftArc(DependencyParsingArcLabel('DET, 'DT)),
        ArcHybridShift,
        ArcHybridLeftArc(),
        LabelLeftArc(DependencyParsingArcLabel('NSUBJ, 'NN)),
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridRightArc()
      )
    )
    interpretation1(state.get, LabelRightArc(DependencyParsingArcLabel('POBJ, 'DT))) shouldBe true
    interpretation2(state.get, LabelRightArc(DependencyParsingArcLabel('POBJ, 'DT))) shouldBe false
    interpretation3(state.get, LabelRightArc(DependencyParsingArcLabel('POBJ, 'DT))) shouldBe false
    interpretation4(state.get, LabelRightArc(DependencyParsingArcLabel('POBJ, 'DT))) shouldBe false
  }
}

