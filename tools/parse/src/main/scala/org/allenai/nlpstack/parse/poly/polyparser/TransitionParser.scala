package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{Token, NexusToken, Sentence}
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint

/** A TransitionParser implements a parsing algorithm for a transition-based parser. */
abstract class TransitionParser {

  /** Given an initial state, this returns the "best" sequence of Transitions that one should
    * apply to the initial state in order to get a polytree parse. The adjective "best" depends
    * on the specific implementation of the TransitionParser.
    *
    * @param sentence the sentence you want to parse
    * @param constraints a set of ParserConstraint objects, which constrain the choices the parser
    *              may make
    * @return the "best" list of Transitions you should apply to the initial state (or None if
    *   a parse failure occurs)
    */
  def parse(sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()): Option[PolytreeParse]

  def parseStringSequence(tokens: Seq[String],
    constraints: Set[TransitionConstraint] = Set()): Option[PolytreeParse] = {

    // note that we invert this back into a rooted tree for this function, since that's what's
    // expected by nlpstack
    parse(Sentence((NexusToken +: (tokens map { tok => Token(Symbol(tok))})).toIndexedSeq),
      constraints) map { x => PolytreeParse.arcInverterStanford(x) }
  }
}
