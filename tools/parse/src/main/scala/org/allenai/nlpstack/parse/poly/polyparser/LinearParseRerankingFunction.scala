package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.LinearModel

/** Scores parses based on a linear combination of features. */
case class LinearParseRerankingFunction(feature: PolytreeParseFeature,
  linearModel: Option[LinearModel]) extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse =>
        linearModel.get.score(feature(parse, baseCost))
      case _ => Double.PositiveInfinity
    }
  }
}

