package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ FactorieSentenceTagger, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._

/** Uses the parser model to create an n-best list, then chooses the best parse from this n-best
  * list (according to the reranking function).
  *
  * @param config configuration object for the parser
  */
case class RerankingTransitionParser(val config: ParserConfiguration) extends TransitionParser {

  @transient val reranker: Reranker = new Reranker(config.rerankingFunction)

  def parse(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[PolytreeParse] = {

    val parsingCostFunction =
      config.parsingCostFunctionFactory.buildCostFunction(sentence, constraints)
    val baseParser = new NbestSearch(parsingCostFunction)
    val nbestList: Option[NbestList] =
      parsingCostFunction.transitionSystem.initialState(
        constraints.toSeq
      ) map { initState =>
        // Only do full reranking in the absence of constraints.
        if (constraints.isEmpty) {
          baseParser.find(initState, config.parsingNbestSize, constraints)
        } else {
          baseParser.find(initState, 1, constraints)
        }
      }
    val mappedNbestList: Option[NbestList] = nbestList map { x =>
      NbestList(x.scoredSculptures)
    }
    val candidate: Option[Sculpture] = mappedNbestList flatMap { nbList => reranker(nbList) }
    candidate match {
      case Some(parse: PolytreeParse) =>

        val mappedParse = parse.copy(sentence = Sentence(
          FactorieSentenceTagger.transform(parse.sentence).tokens map { tok =>
            tok.updateProperties(Map('cpos -> Set(tok.getDeterministicProperty('autoCpos))))
          }
        ))
        Some(mappedParse)
      case _ => None
    }
  }
}
