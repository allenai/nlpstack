package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

/** A struct contains the "modes" of a typical transition parser.
  *
  * Specifically, the mode tells you what the next expected action is.
  */
object DependencyParserModes {
  val TRANSITION: Int = 0
  val LEFTLABEL: Int = 1
  val RIGHTLABEL: Int = 2
}

abstract class DependencyParsingTransitionSystem(
    marbleBlock: MarbleBlock,
    brownClusters: Seq[BrownClusters] = Seq()
) extends TransitionSystem {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcHybridTaskIdentifier

  @transient val sentence: Sentence =
    marbleBlock match {
      case parse: PolytreeParse =>
        parse.sentence
      case sentence: Sentence =>
        sentence
    }

  override def initialState(
    constraints: Seq[TransitionConstraint]
  ): Option[State] = {
    Some(systemSpecificInitialState(sentence))
  }

  protected def systemSpecificInitialState(sentence: Sentence): State

  override def toSculpture(state: State): Option[Sculpture] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.asSculpture
      case _ =>
        None
    }
  }
}

object DependencyParsingTransitionSystem {

  val keywords = WordClusters.commonWords ++ WordClusters.puncWords ++
    WordClusters.stopWords
}

/** The LabelLeftArc operator labels the most recently created left-facing arc. */
case class LabelLeftArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.LEFTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.inverseArcLabels.contains(label)) {
      val gretelChildren: Set[Int] =
        state.children.getOrElse(gretel, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (gretel -> (gretelChildren + crumb))
      )
    } else {
      val crumbChildren: Set[Int] =
        state.children.getOrElse(crumb, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (crumb -> (crumbChildren + gretel))
      )
    }
  }

  @transient
  override val name: String = s"LtLbl[${label.name}]"
}

/** The LabelRightArc operator labels the most recently created right-facing arc. */
case class LabelRightArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.RIGHTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.inverseArcLabels.contains(label)) {
      val gretelChildren: Set[Int] =
        state.children.getOrElse(gretel, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (gretel -> (gretelChildren + crumb))
      )
    } else {
      val crumbChildren: Set[Int] =
        state.children.getOrElse(crumb, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (crumb -> (crumbChildren + gretel))
      )
    }
  }

  @transient
  override val name: String = s"RtLbl[${label.name}]"
}
