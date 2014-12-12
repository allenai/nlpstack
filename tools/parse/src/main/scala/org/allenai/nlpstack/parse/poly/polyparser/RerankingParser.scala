package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.fsm._

/** Uses the parser model to create an n-best list, then chooses the best parse from this n-best
  * list (according to the reranking function).
  *
  * @param config configuration object for the parser
  */
class RerankingTransitionParser(val config: ParserConfiguration) extends TransitionParser {

  val baseParser: NbestSearch = new NbestSearch(config.parsingCostFunction)
  val reranker: Reranker = new Reranker(config.rerankingFunction)

  def parse(sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()): Option[PolytreeParse] = {

    val nbestList: Option[NbestList] =
      config.parsingCostFunction.transitionSystem.initialState(sentence) map { initState =>
        baseParser.find(initState, config.parsingNbestSize, constraints)
      }
    val mappedNbestList: Option[NbestList] = nbestList map { x =>
      NbestList(x.scoredSculptures map { case (sculpture, cost) =>
        (sculpture match {
          case parse: PolytreeParse => PolytreeParse.arcInverterStanford(parse)
          case y => y
        }, cost)
      })
    }
    val candidate: Option[Sculpture] = mappedNbestList flatMap { nbList => reranker(nbList) }
    candidate match {
      case Some(parse: PolytreeParse) =>
        val mappedParse = parse.copy(sentence = Sentence(parse.sentence.tokens map { tok =>
          tok.updateProperties(Map('cpos -> Set(tok.getDeterministicProperty('factorieCpos))))
        }))
        //println(parse)
        Some(mappedParse)
      case _ => None
    }
  }
}
