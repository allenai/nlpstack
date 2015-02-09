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
    * |
    * |       the_1--
    * |              \
    * |               -->cat_2
    * \              /
    * -----> sat_3--
    * /
    * by_4 --
    * \
    * --> me_5
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
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3,5), Set()),
    arclabels = Vector(Set((3, 'ROOT)), Set((2, 'DET)), Set((1, 'DET), (3, 'NSUBJ)),
      Set((0, 'ROOT), (2, 'NSUBJ), (4, 'PREP)), Set((3, 'PREP), (5, 'POBJ)), Set((4, 'POBJ)))
  )

  /** This represents the following parse:
    * format: OFF
    *
    * NEXUS_0
    * |
    * V
    * a_1
    * / \
    * /   \
    * V    V
    * b_2   d_4
    * |
    * V
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
      Set((1, 'n2a)),
      Set((0, 'n2a), (2, 'a2b), (4, 'a2d)),
      Set((1, 'a2b), (3, 'b2c)),
      Set((2, 'b2c)),
      Set((1, 'a2d))
    )
  )

  "Calling GuidedCostFunction's apply" should "duplicate the steps needed to create parse1" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse1.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions} shouldBe Some(List(
      ArcHybridShift,
      ArcHybridLeftArc(),
      LeftLabelArc('DET),
      ArcHybridShift,
      ArcHybridLeftArc(),
      LeftLabelArc('NSUBJ),
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridRightArc(),
      RightLabelArc('POBJ),
      ArcHybridRightArc(),
      RightLabelArc('PREP),
      ArcHybridLeftArc(),
      LeftLabelArc('ROOT),
      ArcHybridShift
    ))
  }

  it should "duplicate the steps needed to create parse2" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse2.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    val greedySearch = new GreedySearch(costFunction)
    greedySearch.find(initialState, Set()) map { walk => walk.transitions} shouldBe Some(List(
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridShift,
      ArcHybridRightArc(),
      RightLabelArc('b2c),
      ArcHybridRightArc(),
      RightLabelArc('a2b),
      ArcHybridShift,
      ArcHybridRightArc(),
      RightLabelArc('a2d),
      ArcHybridLeftArc(),
      LeftLabelArc('n2a),
      ArcHybridShift
    ))
  }

  it should "recreate parse1" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse1.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    val greedySearch = new GreedySearch(costFunction)
    val finalState = greedySearch.find(initialState, Set()) flatMap { walk => walk.finalState }
    val finalSculpture = finalState flatMap { state => transitionSystem.toSculpture(state) }
    //finalSculpture.get.toString shouldBe parse1.toString
    finalSculpture map { sculpture => sculpture match {
      case polyparse: PolytreeParse =>
        polyparse.breadcrumb shouldBe parse1.breadcrumb
        polyparse.children shouldBe parse1.children
        polyparse.arclabels shouldBe parse1.arclabels
    }}
  }

  it should "recreate parse2" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse2.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    val greedySearch = new GreedySearch(costFunction)
    val finalState = greedySearch.find(initialState, Set()) flatMap { walk => walk.finalState }
    val finalSculpture = finalState flatMap { state => transitionSystem.toSculpture(state) }
    //finalSculpture.get.toString shouldBe parse2.toString
    finalSculpture map { sculpture => sculpture match {
      case polyparse: PolytreeParse =>
        polyparse.breadcrumb shouldBe parse2.breadcrumb
        polyparse.children shouldBe parse2.children
        polyparse.arclabels shouldBe parse2.arclabels
    }}
  }

  "The ForbiddenEdge's interpretation" should "return true for a left arc" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val interpretation = ArcHybridForbiddenArcInterpretation(ForbiddenEdge(2, 3))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(parse1, Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LeftLabelArc('DET),
        ArcHybridShift
      )
    )
    interpretation(state.get, ArcHybridLeftArc()) shouldBe true
    interpretation(state.get, ArcHybridRightArc()) shouldBe false
  }

  it should "return true for a right arc" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val interpretation = ArcHybridForbiddenArcInterpretation(ForbiddenEdge(4, 5))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(parse1, Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LeftLabelArc('DET),
        ArcHybridShift,
        ArcHybridLeftArc(),
        LeftLabelArc('NSUBJ),
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridShift
      )
    )
    interpretation(state.get, ArcHybridLeftArc()) shouldBe false
    interpretation(state.get, ArcHybridRightArc()) shouldBe true
  }

  "The ForbiddenArcLabel's interpretation" should "return true for a mislabeled left arc" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val interpretation1 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 2, 'DET))
    val interpretation2 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 3, 'DET))
    val interpretation3 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(0, 2, 'DET))
    val interpretation4 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(1, 2, 'POBJ))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(parse1, Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc()
      )
    )
    interpretation1(state.get, LeftLabelArc('DET)) shouldBe true
    interpretation2(state.get, LeftLabelArc('DET)) shouldBe false
    interpretation3(state.get, LeftLabelArc('DET)) shouldBe false
    interpretation4(state.get, LeftLabelArc('DET)) shouldBe false
  }

  it should "return true for a mislabeled right arc" in {
    val transitionSystem = ArcHybridTransitionSystem()
    val interpretation1 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 5, 'POBJ))
    val interpretation2 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 3, 'POBJ))
    val interpretation3 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(3, 5, 'POBJ))
    val interpretation4 = ArcHybridForbiddenArcLabelInterpretation(ForbiddenArcLabel(4, 5, 'DET))
    val state = StateTransition.applyTransitionSequence(
      transitionSystem.initialState(parse1, Seq()).get,
      List(
        ArcHybridShift,
        ArcHybridLeftArc(),
        LeftLabelArc('DET),
        ArcHybridShift,
        ArcHybridLeftArc(),
        LeftLabelArc('NSUBJ),
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridShift,
        ArcHybridRightArc()
      )
    )
    interpretation1(state.get, RightLabelArc('POBJ)) shouldBe true
    interpretation2(state.get, RightLabelArc('POBJ)) shouldBe false
    interpretation3(state.get, RightLabelArc('POBJ)) shouldBe false
    interpretation4(state.get, RightLabelArc('POBJ)) shouldBe false
  }
}

