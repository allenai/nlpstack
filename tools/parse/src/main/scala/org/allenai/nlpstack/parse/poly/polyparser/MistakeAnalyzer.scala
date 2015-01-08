package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm._


case class ParserMistake(sentence: String, state: State, goldTransition: StateTransition,
                         guessedTransitionCosts: Map[StateTransition, Double]) {

  @transient val guessedTransition = (guessedTransitionCosts minBy { case (_, cost) => cost })._1

  override def toString(): String = {
    s"$sentence\n$state\nguessedCosts: ${guessedTransitionCosts.toSeq.sortBy{_._2}.take(3)}"
  }
}

case class MistakeAnalyzer(costFunction: StateCostFunction, goldParses: PolytreeParseSource,
                           transitionSystem: TransitionSystem) {

  def enumerate(): Unit = {
    val states = for {
      goldParse <- goldParses.parseIterator
      (state, _) <- convertToTransitionParserStates(goldParse)
    } yield (state, goldParse.sentence.asWhitespaceSeparatedString)
    val goldTransitions = for {
      goldParse <- goldParses.parseIterator
      (_, transition) <- convertToTransitionParserStates(goldParse)
    } yield transition
    val guessedTransitions = for {
      goldParse <- goldParses.parseIterator
      (state, _) <- convertToTransitionParserStates(goldParse)
    } yield costFunction(state)
    val triples: Iterator[((State, String), (StateTransition, Map[StateTransition, Double]))] =
      (states zip (goldTransitions zip guessedTransitions))
    val mistakes: Iterator[ParserMistake] = triples flatMap {
      case ((state, sentence), (goldTransition, guessedTransitionCosts)) =>
        if (goldTransition != guessedTransitionCosts.minBy{ _._2 }._1) {
          Some(ParserMistake(sentence, state, goldTransition, guessedTransitionCosts))
        } else {
          None
        }
    }
    val mistakeMap = mistakes.toSeq groupBy { mistake =>
      (mistake.goldTransition, mistake.guessedTransition)
    }
    mistakeMap.keys foreach { case (gold, guessed) =>
      println("---")
      println(s"($gold, $guessed): ${mistakeMap((gold, guessed)).size}")
      mistakeMap((gold, guessed)) foreach println
    }
  }

  private def convertToTransitionParserStates(goldParse: PolytreeParse): Seq[(State, StateTransition)] = {
    transitionSystem.guidedCostFunction(goldParse) match {
      case Some(costFunc) =>
        val search = new GreedySearch(costFunc)
        val initialState = transitionSystem.initialState(goldParse.sentence)
        val bestWalk: Option[Walk] = initialState flatMap { initState =>
          search.find(initState, Set())
        }
        bestWalk match {
          case Some(walk) => walk.states zip walk.transitions
          case None => Seq()
        }
      case None => Seq()
    }
  }
}
