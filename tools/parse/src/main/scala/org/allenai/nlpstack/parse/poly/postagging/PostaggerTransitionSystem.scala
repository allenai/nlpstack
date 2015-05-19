package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser._

/** A simple finite-state transition system for POS-tagging a sentence.
  *
  * In the initial state, all tokens are untagged.
  * In any intermediate state, the first K tokens are tagged, and the last N-K tokens are untagged.
  * A final state has all tokens tagged.
  *
  * There is only one transition operator, called AssignTag(TAG). When applied to an
  * intermediate state, this transition operator assigns the tag TAG to the
  * K+1th token.
  *
  * @param marbleBlock the untagged sentence (must be an instance of either
  * org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse or
  * org.allenai.nlpstack.parse.poly.core.Sentence)
  * @param taggers sentence transformations to use for features
  */
class PostaggerTransitionSystem(
    marbleBlock: MarbleBlock,
    taggers: Seq[SentenceTransform]
) extends TransitionSystem {

  @transient val sentence: Sentence =
    marbleBlock match {
      case parse: PolytreeParse =>
        parse.sentence
      case sentence: Sentence =>
        sentence
    }

  val taskIdentifier: TaskIdentifier = SimpleTaskIdentifier("tagging")

  def initialState(constraints: Seq[TransitionConstraint]): Option[State] = {
    Some(PostaggerState(
      nextTokenToTag =
      if (sentence.tokens.size > 1) {
        Some(1)
      } else {
        None
      },
      existingTags = Map(),
      sentence = sentence
    ))
  }

  def toSculpture(state: State): Option[Sculpture] = state.asSculpture

  def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {

    new TrivialConstraintInterpretation
  }

  def guidedCostFunction(goldObj: Sculpture): Option[StateCostFunction] =
    goldObj match {
      case sentence: TaggedSentence =>
        Some(new PostaggerGuidedCostFunction(sentence, this))
      case _ => None
    }

  private val annotatedSentence: AnnotatedSentence = {
    val taggedSentence = taggers.foldLeft(sentence)((sent, tagger) => tagger.transform(sent))
    val tokenFeatureTagger = new TokenFeatureTagger(Seq(
      TokenPositionFeature,
      TokenPropertyFeature('lexical),
      TokenPropertyFeature('wiki),
      TokenPropertyFeature('posTagFreq1to5),
      TokenPropertyFeature('posTagFreq6to20),
      TokenPropertyFeature('posTagFreq21to50),
      TokenPropertyFeature('posTagFreq51to95),
      TokenPropertyFeature('posTagFreq96to100),
      TokenPropertyFeature('posTagMostLikely),
      TokenPropertyFeature('cposTagFreq1to5),
      TokenPropertyFeature('cposTagFreq6to20),
      TokenPropertyFeature('cposTagFreq21to50),
      TokenPropertyFeature('cposTagFreq51to95),
      TokenPropertyFeature('cposTagFreq96to100),
      TokenPropertyFeature('cposTagMostLikely),
      SuffixFeature(WordClusters.suffixes.toSeq),
      KeywordFeature(DependencyParsingTransitionSystem.keywords)
    ))
    tokenFeatureTagger.tag(taggedSentence)
  }

  override def computeFeature(state: State): FeatureVector = {
    defaultFeature(state)
  }

  private val defaultFeature = FeatureUnion(List(
    new OfflineTokenFeature(annotatedSentence, OffsetRef(0)),
    new OfflineTokenFeature(annotatedSentence, OffsetRef(1)),
    new OfflineTokenFeature(annotatedSentence, OffsetRef(2)),
    new OfflineTokenFeature(annotatedSentence, OffsetRef(-1)),
    new OfflineTokenFeature(annotatedSentence, OffsetRef(-2))
  //new TokenTransformFeature(StackRef(0), Set(GuessedArcLabel, GuessedCpos)), TODO: add GuessedPOSTag
  ))
}

/** A state of the PostaggerTransitionSystem (see above).
  *
  * @param nextTokenToTag indicates the next token that needs tagging
  * (None if all tokens are tagged)
  * @param existingTags tags that have already been assigned
  * @param sentence the sentence we want to tag
  */
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

/** The main transition operator of the PostaggerTransitionSystem (see above).
  *
  * @param tag the tag to assign to the next token
  */
case class AssignTag(tag: Symbol) extends StateTransition {

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

/** The PostaggerGuidedCostFunction uses a gold tagged sentence to make deterministic decisions
  * about which transition to apply in any given state. Since the decision is uniquely determined
  * by the gold tagging, the returned map will have only a single mapping that assigns zero cost
  * to the correct transition (all other transitions therefore have an implicit cost of infinity).
  *
  * @param taggedSentence the gold tagged sentence
  * @param transitionSystem pointer to the transition system
  */
class PostaggerGuidedCostFunction(
    taggedSentence: TaggedSentence,
    override val transitionSystem: TransitionSystem
) extends StateCostFunction {

  override def apply(state: State): Map[StateTransition, Float] = {
    val result: Map[StateTransition, Float] = state match {
      case taggerState: PostaggerState =>
        require(!taggerState.isFinal, "Cannot advance a final state.")
        val nextToTag = taggerState.nextTokenToTag.get
        require(taggedSentence.tags.contains(nextToTag), s"Missing gold tag for token $nextToTag.")
        Map(AssignTag(taggedSentence.tags(nextToTag).head) -> 0)
    }
    result
  }
}

/** Creates a PostaggerTransitionSystem for a given untagged sentence.
  *
  * @param taggers sentence transformations to use for features
  */
case class PostaggerTransitionSystemFactory(
    taggers: Seq[SentenceTransform]
) extends TransitionSystemFactory {

  override def buildTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint]
  ): TransitionSystem = {
    new PostaggerTransitionSystem(marbleBlock, taggers)
  }
}
