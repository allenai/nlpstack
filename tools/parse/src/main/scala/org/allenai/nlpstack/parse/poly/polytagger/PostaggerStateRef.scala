package org.allenai.nlpstack.parse.poly.polytagger

import org.allenai.nlpstack.parse.poly.fsm.State
import org.allenai.nlpstack.parse.poly.polyparser.StateRef

/** A PostaggerStateRef allows you to figure out the token that corresponds to a particular
  * aspect of a PostaggerState.
  *
  * For instance, we may want to know the token to the left of the next token to tag.
  * Applying OffsetRef(-1) to the state will return the index of that token.
  * More accurately, a sequence is returned, which will be empty if the StateRef refers
  * to a non-existent element of the state.
  *
  * This set of classes is used primarily to facilitate feature creation (e.g. see
  * StateRefFeature).
  */
abstract class PostaggerStateRef extends StateRef {

  // Checks that the state is a PostaggerState, otherwise returns an empty sequence.
  override def apply(state: State): Seq[Int] = {
    state match {
      case taggerState: PostaggerState =>
        applyToTaggerState(taggerState)
      case _ =>
        Seq()
    }
  }

  /** Does the main work of the .apply function. Must be implemented by subclasses. */
  protected def applyToTaggerState(state: PostaggerState): Seq[Int]
}

/** The OffsetRef tells us the Kth token to the left or right of the current token (i.e. the next
  * token to tag).
  *
  * @param offset +K means we want the Kth token to the right, -K means we want the Kth token
  * to the left, 0 means we want the current token (i.e. the next token to tag)
  */
case class OffsetRef(offset: Int) extends PostaggerStateRef {

  override def applyToTaggerState(state: PostaggerState): Seq[Int] = {
    require(!state.isFinal, s"Cannot apply an OffsetRef to a final state: $state")
    val desiredToken = state.nextTokenToTag.get + offset
    if (desiredToken >= 0 && desiredToken < state.sentence.tokens.size) {
      Seq(desiredToken)
    } else {
      Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol(s"off$offset")
}
