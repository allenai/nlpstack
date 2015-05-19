package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.core.Postagger
import org.allenai.nlpstack.parse.poly.core.{ WordClusters, SentenceTransform, Sentence }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint

/** Wrapper for a postagger from NLPStack.
  *
  * @param baseTagger the underlying NLPStack postagger
  * @param useCoarseTags set to true if you want the tagger to produce Google coarse POS tags
  */
case class NLPStackPostagger(baseTagger: Postagger, useCoarseTags: Boolean) extends PolyPostagger {

  // TODO: currently no constraints are considered
  override def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence] = {

    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, baseTagger)
    val tagMap = (taggedTokens.zipWithIndex map {
      case (tok, tokIndex) =>
        (tokIndex + 1, Set(Symbol(
          if (useCoarseTags) {
            WordClusters.ptbToUniversalPosTag.getOrElse(tok.postag, "X")
          } else {
            tok.postag
          }
        )))
    }).toMap
    Some(TaggedSentence(sentence, tagMap))
  }
}
