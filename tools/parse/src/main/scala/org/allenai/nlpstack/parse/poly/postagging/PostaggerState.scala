package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.{ TaggedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.fsm.{ StateTransition, Sculpture, State }
import org.allenai.nlpstack.parse.poly.polyparser.StateRef

case class PostaggerState(
    nextTokenToTag: Option[Int],
    existingTags: Map[Int, Symbol],
    sentence: Sentence
) extends State {

  val isFinal: Boolean = {
    nextTokenToTag match {
      case Some(_) => false
      case None => true
    }
  }

  def asSculpture: Option[Sculpture] = {
    if (isFinal) {
      Some(TaggedSentence(sentence, existingTags mapValues { tag =>
        Set(tag)
      }))
    } else {
      None
    }
  }
}

abstract class PostaggerStateRef extends StateRef {

  override def apply(state: State): Seq[Int] = {
    state match {
      case taggerState: PostaggerState =>
        applyToTaggerState(taggerState)
      case _ =>
        Seq()
    }
  }

  def applyToTaggerState(state: PostaggerState): Seq[Int]
}

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
