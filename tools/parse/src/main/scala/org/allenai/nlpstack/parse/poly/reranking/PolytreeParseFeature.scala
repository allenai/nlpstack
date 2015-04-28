package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.ml.{
  FeatureName => MLFeatureName,
  FeatureVector => MLFeatureVector
}
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

import reming.LazyFormat
import reming.DefaultJsonProtocol._

/** Maps a scored parse into a feature vector. */
abstract class PolytreeParseFeature extends ((PolytreeParse, Double) => MLFeatureVector)

object PolytreeParseFeature {
  implicit object PolytreeParseFeatureJsonFormat extends LazyFormat[PolytreeParseFeature] {
    implicit val polytreeParseFeatureUnionFormat = jsonFormat1(PolytreeParseFeatureUnion.apply)
    implicit val baseParserScoreFeatureFormat = jsonFormat0(() => BaseParserScoreFeature)
    implicit val sentenceLengthFeatureFormat = jsonFormat0(() => SentenceLengthFeature)

    override val delegate = parentFormat[PolytreeParseFeature](
      childFormat[PolytreeParseFeatureUnion, PolytreeParseFeature],
      childFormat[BaseParserScoreFeature.type, PolytreeParseFeature],
      childFormat[SentenceLengthFeature.type, PolytreeParseFeature]
    )
  }
}

/** Simply passes along the length of the sentence as a feature. */
case object SentenceLengthFeature extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    MLFeatureVector(Seq(MLFeatureName(List(name)) -> parse.sentence.tokens.tail.size))
  }

  val name: Symbol = 'sentLen
}

/** Simply passes along the original score of the parse as a feature. */
case object BaseParserScoreFeature extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    MLFeatureVector(Seq(MLFeatureName(List(name)) -> score))
  }

  val name: Symbol = 'baseParserScore
}

/** A PolytreeParseFeatureUnion merges the output of a list of features.
  *
  * @param features a list of the features we want to merge into a single feature
  */
case class PolytreeParseFeatureUnion(
    val features: Seq[PolytreeParseFeature]
) extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    features map (f => f(parse, score)) reduce ((m1, m2) => MLFeatureVector.mergeVectors(m1, m2))
  }
}
