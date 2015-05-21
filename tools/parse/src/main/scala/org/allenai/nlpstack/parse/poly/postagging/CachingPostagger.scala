package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.fsm._

/*
case class CachingPostagger(
  fallbackTagger: PolyPostagger,
  sentencesToCache: TaggedSentenceSource)
extends PolyPostagger {

  private def getSentenceKey(sentence: Sentence): String = {
    sentence.asWhitespaceSeparatedString
  }

  private val cache: Map[String, TaggedSentence] = {
    (sentencesToCache.taggedSentenceIterator map { taggedSent =>
      (getSentenceKey(taggedSent.sentence), taggedSent)
    }).toMap
  }

  override def tag(
                    sentence: Sentence,
                    constraints: Set[TransitionConstraint] = Set()
                    ): Option[TaggedSentence] = {

    val sentKey = getSentenceKey(sentence)
    if(cache.contains(sentKey)) {
      cache.get(sentKey)
    } else {
      fallbackTagger.tag(sentence, constraints)
    }
  }
}

case class ExistingTaggerCostFunctionFactory(baseTaggerInit: PolyPostaggerInitializer,
                                         transitionSystemFactory: TransitionSystemFactory)
  extends StateCostFunctionFactory {

  override def buildCostFunction(
                         marbleBlock: MarbleBlock,
                         constraints: Set[TransitionConstraint]
                         ): StateCostFunction = {
    val transitionSystem = transitionSystemFactory.buildTransitionSystem(marbleBlock, constraints)
    val baseTagger = PolyPostagger.initializePostagger(baseTaggerInit)
    new ExistingTaggerCostFunction(baseTagger, transitionSystem, marbleBlock)
  }
}

class ExistingTaggerCostFunction(
  baseTagger: PolyPostagger,
  override val transitionSystem: TransitionSystem,
  marbleBlock: MarbleBlock
) extends StateCostFunction {

  private val taggedSentence: TaggedSentence = {
    val maybeTaggedSentence = marbleBlock match {
      case sentence: Sentence =>
        baseTagger.tag(sentence, Set())
      case _ =>
        None
    }
    require(maybeTaggedSentence != None, s"Marble block $marbleBlock not an accepted type for " +
      s"ExistingTaggerCostFunction")
    maybeTaggedSentence.get
  }

  override def apply(state: State): Map[StateTransition, Float] = {
    state match {
      case taggerState: PostaggerState =>
        require(!taggerState.isFinal, "Cannot advance a final state.")
        val nextToTag = taggerState.nextTokenToTag.get
        require(taggedSentence.tags.contains(nextToTag), s"Missing gold tag for token $nextToTag.")
        Map(AssignTag(taggedSentence.tags(nextToTag).head) -> 0)
    }
  }
}

*/
