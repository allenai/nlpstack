package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, TaggedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.fsm.{ StateTransition, Sculpture, State }
import org.allenai.nlpstack.parse.poly.polyparser.{ StateRef }

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
        Set(
          tag
        //Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(tag.name, "X"))
        )
      }))
    } else {
      None
    }
  }
}

case class TagToken(tag: Symbol) extends StateTransition {

  @transient override val name: String = s"Tag[${tag.name}]"

  override def apply(state: Option[State]): Option[State] = {
    state filter {
      case tpState: PostaggerState => true
      case _ => false
    } map {
      case tpState: PostaggerState => advanceState(tpState)
    }
  }

  private def advanceState(state: PostaggerState): PostaggerState = {
    require(state.nextTokenToTag != None, s"Cannot advance a final state: $state")
    val currentTokenToTag = state.nextTokenToTag.get
    val nextTokenToTag =
      if (currentTokenToTag + 1 < state.sentence.tokens.size) {
        Some(currentTokenToTag + 1)
      } else {
        None
      }
    val revisedTags = state.existingTags.updated(currentTokenToTag, tag)
    state.copy(nextTokenToTag = nextTokenToTag, existingTags = revisedTags)
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
