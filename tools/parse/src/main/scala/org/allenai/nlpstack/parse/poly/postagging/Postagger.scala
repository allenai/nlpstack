package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.{ TaggedSentence, FactorieSentenceTagger, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.polyparser.{ PolytreeParse, TransitionParser, ParserConfiguration }

case class Postagger(
    costFunctionFactory: StateCostFunctionFactory,
    rerankingFunction: RerankingFunction, nbestSize: Int
) {

  @transient val reranker: Reranker = new Reranker(rerankingFunction)

  def tagWithScore(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[(TaggedSentence, Double)] = {

    val costFunction =
      costFunctionFactory.buildCostFunction(sentence, constraints)
    val baseParser = new NbestSearch(costFunction)
    val nbestList: Option[NbestList] =
      costFunction.transitionSystem.initialState(
        constraints.toSeq
      ) map { initState =>
        // Only do full reranking in the absence of constraints.
        if (constraints.isEmpty) {
          baseParser.find(initState, nbestSize, constraints)
        } else {
          baseParser.find(initState, 1, constraints)
        }
      }
    val mappedNbestList: Option[NbestList] = nbestList map { x =>
      NbestList(x.scoredSculptures)
    }
    val candidate: Option[(Sculpture, Double)] = mappedNbestList flatMap { nbList =>
      reranker.rerankWithScore(nbList)
    }
    candidate match {
      case Some((taggedSentence: TaggedSentence, cost)) =>
        Some((taggedSentence, cost))
      case _ => None
    }
  }

  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence] = {

    tagWithScore(sentence, constraints) map { case (tagged, _) => tagged }
  }
}
