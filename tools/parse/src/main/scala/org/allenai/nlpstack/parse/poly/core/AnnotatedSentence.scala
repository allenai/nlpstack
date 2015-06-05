package org.allenai.nlpstack.parse.poly.core

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }
import reming.DefaultJsonProtocol._

/** An AnnotatedSentence is a sentence whose tokens are each annotated with a feature
  * vector.
  *
  * @param sentence the unannotated sentence
  * @param annotation an indexed sequence, of which the nth element is the feature vector for
  * the nth token of the sentence
  */
case class AnnotatedSentence(sentence: Sentence, annotation: IndexedSeq[FeatureVector])

object AnnotatedSentence {
  implicit val annotatedSentenceJsonFormat = jsonFormat2(AnnotatedSentence.apply)

  /** Converts a TaggedSentence into an AnnotatedSentence by making simple features from
    * the tags.
    *
    * @param tagged the original tagged sentence
    * @return an annotated sentence (with feature vectors derived from the tags)
    */
  def annotate(tagged: TaggedSentence): AnnotatedSentence = {
    AnnotatedSentence(
      tagged.sentence,
      Range(0, tagged.sentence.size) map { tokenIndex =>
        FeatureVector(
          tagged.tags.getOrElse(tokenIndex, Set[TokenTag]()).toSeq
            map { tag =>
              FeatureName(Seq(tag.name, tag.value)) -> 1.0
            }
        )
      }
    )
  }
}
