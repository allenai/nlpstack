package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.{ StateCostFunction, State, StateTransition }

class GuidedCostFunctionSpec extends UnitSpec {
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
    arclabels = Vector(Set((3, 'root)), Set((2, 'det)), Set((1, 'det), (3, 'nsubj)),
      Set((0, 'root), (2, 'nsubj), (4, 'prep)), Set((3, 'prep), (5, 'pobj)), Set((4, 'pobj)))
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
    arclabels = Vector(Set((1, 'n2a)), Set((0, 'n2a), (2, 'a2b), (4, 'a2d)), Set((1, 'a2b)),
      Set((4, 'd2c)), Set((1, 'a2d), (3, 'd2c)))
  )

  "Calling GuidedCostFunction's apply" should "duplicate the steps needed to create parse1" in {
    val transitionSystem = ArcEagerTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse1.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse1).get
    costFunction(initialState) shouldBe Map(ArcEagerShift -> 0)
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift)
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerInvertedLeftArc('det) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerShift -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det), ArcEagerShift)
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerLeftArc('nsubj) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det), ArcEagerShift, ArcEagerLeftArc('nsubj))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerRightArc('root) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det), ArcEagerShift, ArcEagerLeftArc('nsubj), ArcEagerRightArc('root))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerInvertedRightArc('prep) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det), ArcEagerShift, ArcEagerLeftArc('nsubj),
        ArcEagerRightArc('root), ArcEagerInvertedRightArc('prep))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerRightArc('pobj) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerShift, ArcEagerInvertedLeftArc('det), ArcEagerShift, ArcEagerLeftArc('nsubj),
        ArcEagerRightArc('root), ArcEagerInvertedRightArc('prep), ArcEagerRightArc('pobj))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerReduce -> 0))
  }

  it should "duplicate the steps needed to create parse2" in {
    val transitionSystem = ArcEagerTransitionSystem()
    val initialState: State = transitionSystem.initialState(parse2.sentence, Seq()).get
    val costFunction: StateCostFunction = transitionSystem.guidedCostFunction(parse2).get
    costFunction(initialState) shouldBe Map(ArcEagerRightArc('n2a) -> 0)
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerRightArc('a2b) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a), ArcEagerRightArc('a2b))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerShift -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a), ArcEagerRightArc('a2b), ArcEagerShift)
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerLeftArc('d2c) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a), ArcEagerRightArc('a2b), ArcEagerShift, ArcEagerLeftArc('d2c))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerReduce -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a), ArcEagerRightArc('a2b), ArcEagerShift, ArcEagerLeftArc('d2c), ArcEagerReduce)
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerRightArc('a2d) -> 0))
    StateTransition.applyTransitionSequence(
      initialState,
      List(ArcEagerRightArc('n2a), ArcEagerRightArc('a2b), ArcEagerShift, ArcEagerLeftArc('d2c),
        ArcEagerReduce, ArcEagerRightArc('a2d))
    ) map { state =>
        costFunction(state)
      } shouldBe Some(Map(ArcEagerReduce -> 0))
  }

  /*
  "Serializing a CostFunctionSignature" should "preserve the data structure" in {
    val signature: CostFunctionSignature = new CostFunctionSignature(
      ApplicabilitySignatureIdentifier,
      new ArcEagerTransitionSystem(TokenTransformFeature(StackRef(3), WordTransform)),
      IndexedSeq(Shift, Reduce))
    signature.toJson.convertTo[CostFunctionSignature] shouldBe signature
  }
  */

}
