package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint

/** A ForbiddenEdge constraint designates a transition as illegal if it would directly create
  * an arc (in either direction) between the tokens at the given indices.
  *
  * Note that argument order does not matter for the constructor.
  *
  * @param token1 index of the first token
  * @param token2 index of the second token
  */
case class ForbiddenEdge(token1: Int, token2: Int) extends TransitionConstraint

/** A ForbiddenArcLabel constraint designates a transition as illegal if it would directly
  * create an arc (in either direction) with the specified label between the tokens at the given
  * indices. It also implicitly creates a RequestedArc constraint for the specified arc
  * (basically it says that we DO want an arc between the specified indices, just not with this
  * label).
  *
  * Note that argument order (of the token indices) does not matter for the constructor.
  *
  * @param token1 index of the first token
  * @param token2 index of the second token
  * @param arcLabel label that is forbidden between the two tokens
  */
case class ForbiddenArcLabel(token1: Int, token2: Int,
  arcLabel: Symbol) extends TransitionConstraint

/** A RequestedArc constraint requests that the output parse MUST contain the requested arc.
  *
  * The arc is specified using the index of the token at the arc's head followed by the index of
  * the token at the arc's tail.
  *
  * Note: currently this constraint does not pay attention to the arc direction, nor the arc
  * label. It only enforces that that there is some edge between the two specified tokens.
  *
  * @param token1 index of the first token
  * @param token2 index of the second token
  * @param arcLabel desired label for the arc
  */
case class RequestedArc(token1: Int, token2: Int,
  arcLabel: Option[Symbol] = None) extends TransitionConstraint

/** A RequestedCpos constraint specifies the coarse part-of-speech tag of a particular token.
  * This means that in the returned parse, the 'cpos property for that token will correspond
  * to the requested coarse tag.
  *
  * @param tokenIndex index of the desired token
  * @param cpos desired coarse tag for the token
  */
case class RequestedCpos(tokenIndex: Int, cpos: Symbol) extends TransitionConstraint
