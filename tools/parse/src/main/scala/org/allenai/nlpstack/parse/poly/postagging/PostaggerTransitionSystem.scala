package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser._

case class PostaggerTransitionSystemFactory(
    taggers: Seq[SentenceTransform]
) extends TransitionSystemFactory {

  def buildTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint]
  ): TransitionSystem = {
    new PostaggerTransitionSystem(marbleBlock, taggers)
  }
}
class PostaggerTransitionSystem(marbleBlock: MarbleBlock, taggers: Seq[SentenceTransform]) extends TransitionSystem {

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
      TokenPropertyFeature('mostLikelyPos),
      TokenPropertyFeature('posFreqBelow10),
      TokenPropertyFeature('posFreqBelow50),
      TokenPropertyFeature('posFreqBelow95),
      TokenPropertyFeature('posFreqDominant),
      TokenPropertyFeature('mostLikelyTag),
      TokenPropertyFeature('tagFreqBelow10),
      TokenPropertyFeature('tagFreqBelow50),
      TokenPropertyFeature('tagFreqBelow95),
      TokenPropertyFeature('tagFreqDominant),
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
  //new TokenTransformFeature(StackRef(0), Set(GuessedArcLabel, GuessedCpos)),
  ))
}

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
        Map(TagToken(taggedSentence.tags(nextToTag).head) -> 0)
    }
    result
  }
}

