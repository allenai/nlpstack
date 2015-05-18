package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.core.Postagger
import org.allenai.nlpstack.parse.poly.core.{ WordClusters, SentenceTransform, TaggedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.postag._
import reming.DefaultJsonProtocol._

trait PolyPostagger {
  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence]
}

object PolyPostagger {

  def initializePostagger(initializer: PolyPostaggerInitializer): PolyPostagger = {
    initializer match {
      case FactoriePostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(defaultPostagger, useCoarseTags)
      case StanfordPostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(new StanfordPostagger(), useCoarseTags)
      case SimplePostaggerInitializer(configFile) =>
        SimplePostagger.load(configFile)
    }
  }
}

case class NLPStackPostagger(baseTagger: Postagger, useCoarseTags: Boolean) extends PolyPostagger {
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

sealed trait PolyPostaggerInitializer
case class FactoriePostaggerInitializer(useCoarseTags: Boolean) extends PolyPostaggerInitializer
case class StanfordPostaggerInitializer(useCoarseTags: Boolean) extends PolyPostaggerInitializer
case class SimplePostaggerInitializer(configFile: String) extends PolyPostaggerInitializer

object PolyPostaggerInitializer {
  private implicit val factorieInitFormat = jsonFormat1(FactoriePostaggerInitializer.apply)
  private implicit val stanfordInitFormat = jsonFormat1(StanfordPostaggerInitializer.apply)
  private implicit val simpleInitFormat = jsonFormat1(SimplePostaggerInitializer.apply)

  implicit val postaggerInitJsonFormat = parentFormat[PolyPostaggerInitializer](
    childFormat[FactoriePostaggerInitializer, PolyPostaggerInitializer],
    childFormat[StanfordPostaggerInitializer, PolyPostaggerInitializer],
    childFormat[SimplePostaggerInitializer, PolyPostaggerInitializer]
  )
}
