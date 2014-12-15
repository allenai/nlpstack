package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{Sentence, NexusToken, Token}
import org.allenai.nlpstack.parse.poly.fsm.{State, TransitionSystem, StateCostFunction}
import org.allenai.nlpstack.parse.poly.ml.{FeatureName, FeatureVector}
import org.allenai.nlpstack.parse.poly.polyparser.labeler._


class ParseLabelerStateSpec extends UnitSpec {
  // scalastyle:off

  /** This represents the following polytree parse:
    *
    *   NEXUS_0
    *       |
    *       |       the_1--
    *       |              \
    *       |               -->cat_2
    *       \              /
    *        -----> sat_3--
    *          /
    *   by_4 --
    *          \
    *           --> me_5
    *
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(NexusToken, Token('the), Token('cat), Token('sat),
      Token('by), Token('me))),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels = Vector(Set((3, 'root)), Set((2, 'det)), Set((1, 'det), (3, 'nsubj)),
      Set((0, 'root), (2, 'nsubj), (4, 'prep)), Set((3, 'prep), (5, 'pobj)), Set((4, 'pobj))))

  val exampleStates = IndexedSeq(
    ParseLabelerState(parse1, Map(), Some(3)),
    ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root)), Some(2)),
    ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root),
      2 -> ParseLabel('nsubj)), Some(1)),
    ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root),
      2 -> ParseLabel('nsubj),
      1 -> ParseLabel('det)), Some(4)),
    ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root),
      2 -> ParseLabel('nsubj),
      1 -> ParseLabel('det),
      4 -> ParseLabel('prep)), Some(5)),
    ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root),
      2 -> ParseLabel('nsubj),
      1 -> ParseLabel('det),
      4 -> ParseLabel('prep),
      5 -> ParseLabel('pobj)), None)
  )

  "Calling the .apply function of DepthFirstParseNodeSuccessor" should "return the nodes in the " +
    "correct order" in {
    val successor = DepthFirstParseNodeSuccessor(parse1)
    Seq(0, 1, 2, 3, 4, 5, 6) map { x => successor(Some(x)) } shouldBe Seq(
      Some(3), Some(4), Some(1), Some(2), Some(5), None, None)
  }

  "Calling the .apply function of GuidedCostFunctionForLabeler" should "return the correct " +
    "sequence of transitions" in {
    val transitionSystem: TransitionSystem = ParseLabelerTransitionSystem
    val costFunction: StateCostFunction =
      new GuidedCostFunctionForLabeler(parse1, transitionSystem)
    val initialState: State = transitionSystem.initialState(parse1).get
    val stateSequence = IndexedSeq(
      ParseLabelerState(parse1, Map(), Some(3)),
      ParseLabelerState(parse1, Map(
        3 -> ParseLabel('root)), Some(2)),
      ParseLabelerState(parse1, Map(
        3 -> ParseLabel('root),
        2 -> ParseLabel('nsubj)), Some(1)),
      ParseLabelerState(parse1, Map(
        3 -> ParseLabel('root),
        2 -> ParseLabel('nsubj),
        1 -> ParseLabel('det)), Some(4)),
      ParseLabelerState(parse1, Map(
        3 -> ParseLabel('root),
        2 -> ParseLabel('nsubj),
        1 -> ParseLabel('det),
        4 -> ParseLabel('prep)), Some(5)),
      ParseLabelerState(parse1, Map(
        3 -> ParseLabel('root),
        2 -> ParseLabel('nsubj),
        1 -> ParseLabel('det),
        4 -> ParseLabel('prep),
        5 -> ParseLabel('pobj)), None)
    )
    costFunction(initialState) shouldBe Map(AddNodeLabel(ParseLabel('root)) -> 0.0)
    costFunction(stateSequence(0)) shouldBe Map(AddNodeLabel(ParseLabel('root)) -> 0.0)
    costFunction(stateSequence(1)) shouldBe Map(AddNodeLabel(ParseLabel('nsubj)) -> 0.0)
    costFunction(stateSequence(2)) shouldBe Map(AddNodeLabel(ParseLabel('det)) -> 0.0)
    costFunction(stateSequence(3)) shouldBe Map(AddNodeLabel(ParseLabel('prep)) -> 0.0)
    costFunction(stateSequence(4)) shouldBe Map(AddNodeLabel(ParseLabel('pobj)) -> 0.0)
    costFunction(stateSequence(5)) shouldBe Map()
  }

  "Calling the .apply function of AddNodeLabel" should "return the correct " +
    "sequence of states" in {

    AddNodeLabel(ParseLabel('root))(Some(exampleStates(0))) shouldBe Some(exampleStates(1))
    AddNodeLabel(ParseLabel('nsubj))(Some(exampleStates(1))) shouldBe Some(exampleStates(2))
    AddNodeLabel(ParseLabel('det))(Some(exampleStates(2))) shouldBe Some(exampleStates(3))
    AddNodeLabel(ParseLabel('prep))(Some(exampleStates(3))) shouldBe Some(exampleStates(4))
    AddNodeLabel(ParseLabel('pobj))(Some(exampleStates(4))) shouldBe Some(exampleStates(5))
    AddNodeLabel(ParseLabel('root))(Some(exampleStates(5))) shouldBe None
  }

  "Calling the .toSculpture function of a completed state" should "give a correct parse tree" in {
    val state = ParseLabelerState(parse1, Map(
      3 -> ParseLabel('root),
      2 -> ParseLabel('nsubj),
      1 -> ParseLabel('det),
      4 -> ParseLabel('prep),
      5 -> ParseLabel('pobj)), None)
    state.asSculpture shouldBe Some(parse1)
  }

  /*
  "Calling the .apply function of BreadcrumbLabelFeature" should "return the correct value" in {
    val feature = BreadcrumbLabelFeature
    feature(exampleStates(0)) shouldBe FeatureVector(Map(
      FeatureName(List('breadcrumbLabel, 'None)) -> 1.0))
    feature(exampleStates(1)) shouldBe FeatureVector(Map(
      FeatureName(List('breadcrumbLabel, 'root)) -> 1.0))
    feature(exampleStates(2)) shouldBe FeatureVector(Map(
      FeatureName(List('breadcrumbLabel, 'nsubj)) -> 1.0))
    feature(exampleStates(3)) shouldBe FeatureVector(Map(
      FeatureName(List('breadcrumbLabel, 'root)) -> 1.0))
    feature(exampleStates(4)) shouldBe FeatureVector(Map(
      FeatureName(List('breadcrumbLabel, 'prep)) -> 1.0))
    feature(exampleStates(5)) shouldBe FeatureVector(Map())
  }
  */
}
