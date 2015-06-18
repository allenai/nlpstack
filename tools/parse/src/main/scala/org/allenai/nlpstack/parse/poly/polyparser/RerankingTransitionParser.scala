package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.fsm._

/** Uses the parser model to create an n-best list, then chooses the best parse from this n-best
  * list (according to the reranking function).
  *
  * @param config configuration object for the parser
  */
case class RerankingTransitionParser(config: ParserConfiguration) extends TransitionParser {

  @transient val reranker: Reranker = new Reranker(config.rerankingFunction)

  def parseWithScore(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set(),
    doFastApproximation: Boolean = false
  ): Option[(PolytreeParse, Double)] = {

    val parsingCostFunction =
      config.parsingCostFunctionFactory.buildCostFunction(sentence, constraints)
    val baseParser = new NbestSearch(parsingCostFunction)
    val nbestList: Option[NbestList] =
      parsingCostFunction.transitionSystem.initialState(
        constraints.toSeq
      ) map { initState =>
        val nbestSize = // do full reranking only in the absence of constraints
          if (constraints.nonEmpty) { 1 } else if (doFastApproximation) { 2 } else { config.parsingNbestSize }
        baseParser.find(initState, nbestSize, constraints)
      }
    val mappedNbestList: Option[NbestList] = nbestList map { x =>
      NbestList(x.scoredSculptures)
    }
    val candidate: Option[(Sculpture, Double)] = mappedNbestList flatMap { nbList =>
      reranker.rerankWithScore(nbList)
    }
    candidate match {
      case Some((parse: PolytreeParse, cost)) =>
        Some((parse, cost))
      case _ => None
    }
  }
  def parse(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[PolytreeParse] = {

    parseWithScore(sentence, constraints) map { case (parse, _) => parse }
  }
}
