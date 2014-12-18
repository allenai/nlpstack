package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{Sentence, NexusToken, Token}
import org.allenai.nlpstack.parse.poly.fsm.GreedySearch

class GreedySearchSpec extends UnitSpec {
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
    sentence = Sentence(Vector(NexusToken,
      Token('the, Map('cpos -> Set('DT))),
      Token('cat, Map('cpos -> Set('NN))),
      Token('sat, Map('cpos -> Set('VB))),
      Token('by, Map('cpos -> Set('IN))),
      Token('me, Map('cpos -> Set('PRP))))),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels = Vector(Set((3, 'root)), Set((2, 'det)), Set((1, 'det), (3, 'nsubj)),
      Set((0, 'root), (2, 'nsubj), (4, 'prep)), Set((3, 'prep), (5, 'pobj)), Set((4, 'pobj))))

  "Calling GreedySearch's find" should "re-create parse1" in {
    val costFunction = new ArcEagerGuidedCostFunction(parse1, ArcEagerTransitionSystem())
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(costFunction.transitionSystem.initialState(parse1.sentence).get,
      Set())
    bestWalk map { _.transitions } shouldBe Some(List(ArcEagerShift, ArcEagerInvertedLeftArc('det),
      ArcEagerShift, ArcEagerLeftArc('nsubj), ArcEagerRightArc('root),
      ArcEagerInvertedRightArc('prep), ArcEagerRightArc('pobj),
      ArcEagerReduce, ArcEagerReduce, ArcEagerReduce))
  }

  it should "have a failure if we create an impossible constraint" in {
    val costFunction = new ArcEagerGuidedCostFunction(parse1, ArcEagerTransitionSystem())
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(costFunction.transitionSystem.initialState(parse1.sentence).get,
      constraints = Set(ForbiddenEdge(3, 4)))
    bestWalk shouldBe None
  }

  it should "recreate parse1 if we only forbid unnecessary arcs" in {
    val costFunction = new ArcEagerGuidedCostFunction(parse1, ArcEagerTransitionSystem())
    val parser = new GreedySearch(costFunction)
    val bestWalk = parser.find(costFunction.transitionSystem.initialState(parse1.sentence).get,
      constraints = Set(ForbiddenEdge(3, 5), ForbiddenEdge(1, 3)))
    bestWalk map { _.transitions } shouldBe Some(List(ArcEagerShift, ArcEagerInvertedLeftArc('det),
      ArcEagerShift, ArcEagerLeftArc('nsubj), ArcEagerRightArc('root),
      ArcEagerInvertedRightArc('prep), ArcEagerRightArc('pobj),
      ArcEagerReduce, ArcEagerReduce, ArcEagerReduce))
  }

  //TODO: some test that exercises a cost function that returns scores for multiple transitions
}
