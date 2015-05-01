package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{
  AnnotatedSentence,
  Sentence,
  SentenceTransform,
  WordClusters
}
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

/** A compound label for a dependency tree, used by transition systems that inherit from
  * DependencyParsingTransitionSystem.
  *
  * When an arc is labeled with a DependencyParsingArcLabel(stanLabel, cpos), it signifies that
  * the coarse POS of the gretel is `cpos`, while the Stanford dependency label of the arc is
  * stanLabel.
  *
  * @param stanLabel the Stanford dependency label (e.g. 'nsubj, 'dobj) of the arc
  * @param cpos the coarse POS tag of the arc's gretel
  */
case class DependencyParsingArcLabel(stanLabel: Symbol, cpos: Symbol) extends ArcLabel {
  @transient private lazy val symbolicRepresentation = Symbol(stanLabel.name + "::" + cpos.name)

  override def toSymbol: Symbol = symbolicRepresentation

  override def toString: String = symbolicRepresentation.name
}

abstract class DependencyParsingTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint],
    taggers: Seq[SentenceTransform]
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

  val annotatedSentence: AnnotatedSentence = {
    val taggedSentence = taggers.foldLeft(sentence)((sent, tagger) => tagger.transform(sent))

    // override factorie tags with requested tags
    val requestedCposConstraints: Map[Int, RequestedCpos] =
      (constraints flatMap { constraint: TransitionConstraint =>
        constraint match {
          case cposConstraint: RequestedCpos => Some(cposConstraint)
          case _ => None
        }
      } map { constraint =>
        (constraint.tokenIndex, constraint)
      }).toMap
    val overriddenSentence: Sentence = Sentence(
      Range(0, taggedSentence.tokens.size) map { i =>
        requestedCposConstraints.get(i)
      } zip taggedSentence.tokens map {
        case (maybeConstraint, tok) =>
          maybeConstraint match {
            case Some(constraint) =>
              tok.updateProperties(Map(
                'autoCpos -> Set(constraint.cpos),
                'autoPos -> Set()
              ))
            case None => tok
          }
      }
    )
    val tokenFeatureTagger = new TokenFeatureTagger(Seq(
      TokenPositionFeature,
      //TokenPropertyFeature('autoCpos),
      //TokenPropertyFeature('autoPos),
      TokenPropertyFeature('brown0),
      TokenPropertyFeature('mostLikelyTag),
      TokenPropertyFeature('tagFreqBelow10),
      TokenPropertyFeature('tagFreqBelow50),
      TokenPropertyFeature('tagFreqBelow95),
      TokenPropertyFeature('tagFreqDominant),
      //TokenPropertyFeature('mostLikelyPos),
      //TokenPropertyFeature('posFreqBelow5),
      //TokenPropertyFeature('posFreqBelow20),
      //TokenPropertyFeature('posFreqBelow80),
      //TokenPropertyFeature('posFreqDominant),
      //TokenPropertyFeature('verbnetPrimaryFrames),
      //TokenPropertyFeature('verbnetSecondaryFrames),
      KeywordFeature(DependencyParsingTransitionSystem.keywords)
    ))
    tokenFeatureTagger.tag(overriddenSentence)
  }

  val labelingFeature =
    FeatureUnion(List(
      new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), StackRef(2), BufferRef(0),
        BufferRef(1), PreviousLinkCrumbRef, PreviousLinkGretelRef, PreviousLinkCrumbGretelRef,
        PreviousLinkGrandgretelRef,
        TransitiveRef(StackRef(0), Seq(TokenGretels)),
        TransitiveRef(StackRef(1), Seq(TokenGretels)),
        TransitiveRef(BufferRef(0), Seq(TokenGretels)),
        TransitiveRef(StackRef(0), Seq(TokenCrumb)),
        StackLeftGretelsRef(0), StackRightGretelsRef(0))),
      new OfflineTokenFeature(annotatedSentence, StackRef(0)),
      new OfflineTokenFeature(annotatedSentence, StackRef(1)),
      new OfflineTokenFeature(annotatedSentence, StackRef(2)),
      new OfflineTokenFeature(annotatedSentence, BufferRef(0)),
      new OfflineTokenFeature(annotatedSentence, BufferRef(1)),
      new OfflineTokenFeature(annotatedSentence, PreviousLinkCrumbRef),
      new OfflineTokenFeature(annotatedSentence, PreviousLinkGretelRef),
      new OfflineTokenFeature(
        annotatedSentence,
        TransitiveRef(PreviousLinkCrumbRef, Seq(TokenGretels))
      ),
      new OfflineTokenFeature(
        annotatedSentence,
        TransitiveRef(PreviousLinkGretelRef, Seq(TokenGretels))
      ),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(1), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(BufferRef(0), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, StackLeftGretelsRef(0)),
      new OfflineTokenFeature(annotatedSentence, StackRightGretelsRef(0)),
      new OfflineTokenFeature(annotatedSentence, FirstRef),
      new TokenTransformFeature(StackRef(0), Set(GuessedArcLabel, GuessedCpos)),
      new TokenTransformFeature(BufferRef(0), Set(GuessedArcLabel, GuessedCpos)),
      new TokenTransformFeature(StackRef(1), Set(GuessedArcLabel, GuessedCpos)),
      new TokenTransformFeature(TransitiveRef(StackRef(0), Seq(TokenGretels)), Set(GuessedArcLabel)),
      new TokenTransformFeature(TransitiveRef(StackRef(1), Seq(TokenGretels)), Set(GuessedArcLabel)),
      new TokenTransformFeature(TransitiveRef(BufferRef(0), Seq(TokenGretels)), Set(GuessedArcLabel)),
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords)))
    ))

  val defaultFeature = FeatureUnion(List(
    new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), BufferRef(0), BufferRef(1),
      TransitiveRef(StackRef(0), Seq(TokenGretels)),
      TransitiveRef(StackRef(1), Seq(TokenGretels)),
      TransitiveRef(BufferRef(0), Seq(TokenGretels)),
      TransitiveRef(StackRef(0), Seq(TokenCrumb)),
      StackLeftGretelsRef(0), StackRightGretelsRef(1))),
    new OfflineTokenFeature(annotatedSentence, StackRef(0)),
    new OfflineTokenFeature(annotatedSentence, StackRef(1)),
    new OfflineTokenFeature(annotatedSentence, BufferRef(0)),
    new OfflineTokenFeature(annotatedSentence, BufferRef(1)),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(1), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(BufferRef(0), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenCrumb))),
    new OfflineTokenFeature(annotatedSentence, FirstRef),
    new TokenTransformFeature(StackRef(0), Set(GuessedArcLabel, GuessedCpos)),
    new TokenTransformFeature(BufferRef(0), Set(GuessedArcLabel, GuessedCpos)),
    new TokenTransformFeature(StackRef(1), Set(GuessedArcLabel, GuessedCpos)),
    new TokenTransformFeature(TransitiveRef(StackRef(0), Seq(TokenGretels)), Set(GuessedArcLabel)),
    new TokenTransformFeature(TransitiveRef(StackRef(1), Seq(TokenGretels)), Set(GuessedArcLabel)),
    new TokenTransformFeature(TransitiveRef(BufferRef(0), Seq(TokenGretels)), Set(GuessedArcLabel)),
    new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords)))
  ))

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

  def transformArcLabels(parse: PolytreeParse): PolytreeParse = {
    parse.copy(
      arclabels = parse.arclabels.zipWithIndex map {
      case (arcSet, token) =>
        arcSet map {
          case (otherToken, label) =>
            val cposToken = if (parse.breadcrumb(otherToken) == token) {
              otherToken
            } else {
              token
            }
            val cpos = parse.tokens(cposToken).getDeterministicProperty('cpos)
            val revisedArcLabel: ArcLabel = DependencyParsingArcLabel(label.toSymbol, cpos)
            (otherToken, revisedArcLabel)
        }
    }
    )
  }
}

/** The LabelLeftArc operator labels the most recently created left-facing arc. */
case class LabelLeftArc(label: ArcLabel) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.LEFTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.isInvertible(label)) {
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
  override val name: String = s"LtLbl[$label]"
}

/** The LabelRightArc operator labels the most recently created right-facing arc. */
case class LabelRightArc(label: ArcLabel) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.RIGHTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.isInvertible(label)) {
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
  override val name: String = s"RtLbl[$label]"
}
