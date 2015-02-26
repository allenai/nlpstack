package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.eval.ParseScore
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.LinearModel
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

/** Rescores a parse tree according to a specified scoring function.
  *
  * @param scoringFunction the desired scoring function
  */
case class ParseRerankingFunction(scoringFunction: ParseScore)
    extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse => 1.0 - scoringFunction(parse)
      case _ => 1.0
    }
  }
}

/** Rescores a parse tree based on a linear combination of features.
  *
  * @param feature computes a feature vector from the parse tree
  * @param linearModel computes a linear combination of the computed features
  */
case class LinearParseRerankingFunction(
    feature: PolytreeParseFeature,
    linearModel: Option[LinearModel]
) extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse =>
        linearModel.get.score(feature(parse, baseCost))
      case _ => Double.PositiveInfinity
    }
  }
}

