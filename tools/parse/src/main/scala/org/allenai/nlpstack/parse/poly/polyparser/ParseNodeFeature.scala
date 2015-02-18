package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }

/** Maps a selected node of a parse tree into a feature vector. */
abstract class ParseNodeFeature extends ((PolytreeParse, Int) => FeatureVector)

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
      } yield {
        val featureName = List(extractorName, transformName) ++ transform(neighborhood)
        FeatureName(featureName map { x => Symbol(x) }) -> 1.0
      }
    )
  }
}

case object ChildrenArcLabelsFeature extends ParseNodeFeature {
  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(Seq(
      FeatureName('childArcLabels +: (parse.families(token).tail.toList map { tok =>
        parse.arcLabelByEndNodes(Set(tok, token))
      })) -> 1.0
    ))
  }
}

/*
case class TransformedNeighborhoodHistogramFeature(
  neighborhoodExtractors: Seq[(String, NeighborhoodExtractor)],
  transforms: Seq[(String, NeighborhoodTransform)],
  goldParses: PolytreeParseSource
) extends ParseNodeFeature {

  private val histograms: Map[(String, String), Map[Seq[String], Int]] =
    (for {
      (extractorName, extractor) <- neighborhoodExtractors
      (transformName, transform) <- transforms
    } yield {
      var histogram = Map[Seq[String], Int]()
      goldParses.parseIterator foreach { parse =>
        Range(1, parse.tokens.size) foreach { tokenIndex =>
          val event: Seq[String] = transform(extractor(parse, tokenIndex))
          histogram = histogram.updated(event, 1 + histogram.getOrElse(event, 0))
        }
      }
      (extractorName, transformName) -> histogram
    }).toMap

  histograms foreach { case (label, hist) =>
    println(label)
    (hist.toSeq.sortBy { _._2 }) foreach { case (key, value) =>
      println(s"$key: $value")
    }
  }

  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(
      for {
        (extractorName, extractor) <- neighborhoodExtractors
        (transformName, transform) <- transforms
      } yield {
        val eventCount =
          histograms((extractorName, transformName)).getOrElse(transform(extractor(parse, token)), 0)
        if (eventCount > 4) {
          FeatureName(List('count5, Symbol(extractorName), Symbol(transformName))) -> 1.0
        } else {
          FeatureName(List('count5, Symbol(extractorName), Symbol(transformName))) -> 0.0
        }
      }
    )
  }
}
*/
