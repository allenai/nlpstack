package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

import reming.LazyFormat
import reming.DefaultJsonProtocol._

/** Maps a selected node of a parse tree into a feature vector. */
abstract class ParseNodeFeature extends ((PolytreeParse, Int) => FeatureVector)

object ParseNodeFeature {
  implicit object ParseNodeFeatureJsonFormat extends LazyFormat[ParseNodeFeature] {
    private implicit val parseNodeFeatureUnionFormat = jsonFormat1(ParseNodeFeatureUnion.apply)

    private implicit val transformedNeighborhoodFeatureFormat =
      jsonFormat2(TransformedNeighborhoodFeature.apply)

    override val delegate = parentFormat[ParseNodeFeature](
      childFormat[ParseNodeFeatureUnion, ParseNodeFeature],
      childFormat[TransformedNeighborhoodFeature, ParseNodeFeature]
    )
  }
}

/** A ParseNodeFeatureUnion merges the output of a list of features.
  *
  * @param features a list of the features we want to merge into a single feature
  */
case class ParseNodeFeatureUnion(
    features: Seq[ParseNodeFeature]
) extends ParseNodeFeature {

  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    features map (f => f(parse, token)) reduce ((m1, m2) => FeatureVector.mergeVectors(m1, m2))
  }
}

/** A TransformedNeighborhoodFeature creates a feature vector from a set of neighborhood
  * extractors and transforms.
  *
  * @param neighborhoodExtractors the neighborhood extractors you want to apply to each parse node
  * @param transforms the transforms you want to apply to the extracted neighborhoods
  */
case class TransformedNeighborhoodFeature(
    neighborhoodExtractors: Seq[(String, NeighborhoodExtractor)],
    transforms: Seq[(String, NeighborhoodTransform)]
) extends ParseNodeFeature {

  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(
      for {
        (extractorName, extractor) <- neighborhoodExtractors
        neighborhood <- extractor(parse, token)
        (transformName, transform) <- transforms
        transformedNeighborhood <- transform(parse, neighborhood)
      } yield {
        val featureName = (Seq(extractorName, transformName) map { x => Symbol(x) }) ++
          transformedNeighborhood.symbols
        FeatureName(featureName) -> 1.0
      }
    )
  }
}
