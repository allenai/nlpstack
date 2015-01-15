package org.allenai.nlpstack.parse.poly.decisiontree

import spray.json.DefaultJsonProtocol._

case class RandomForest(allOutcomes: Seq[Int], decisionTrees: Seq[DecisionTree])
    extends ProbabilisticClassifier {

  println("decision rules:")
  decisionRules.sortBy { -_._2 } filter { _._2 > 0.02 } foreach println

  def outcomeDistribution2(featureVector: FeatureVector): Map[Int, Double] = {
    val outcomeHistogram = decisionTrees map { decisionTree =>
      decisionTree.classify(featureVector)
    } groupBy { x => x } mapValues { v => v.size }
    RandomForest.convertFrequenciesToDistribution(outcomeHistogram)
  }

  def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    val summedOutcomeHistograms: Map[Int, Int] = decisionTrees flatMap { decisionTree =>
      decisionTree.outcomeHistogram(featureVector).toSeq
    } groupBy { case (x, y) => x } mapValues { case x => x map { _._2 } } mapValues { _.sum }
    RandomForest.convertFrequenciesToDistribution(summedOutcomeHistograms)
  }

  override def allFeatures: Set[Int] = {
    (decisionTrees map { _.allFeatures }) reduce { (x, y) => x ++ y }
  }

  @transient lazy val decisionRules: Seq[(Seq[(Int, Int)], Double)] = {
    (decisionTrees flatMap { decisionTree =>
      decisionTree.decisionPaths zip (decisionTree.distribution map { x => x(1) })
    }).toSet.toSeq
  }
}

object RandomForest {
  implicit val rfFormat = jsonFormat2(RandomForest.apply)

  def convertFrequenciesToDistribution(frequencies: Map[Int, Int]): Map[Int, Double] = {
    val normalizer: Double = frequencies.values.sum
    require(normalizer > 0d)
    frequencies mapValues { _ / normalizer }
  }
}

class RandomForestTrainer(validationPercentage: Double, numDecisionTrees: Int,
    featuresExaminedPerNode: Int) extends ProbabilisticClassifierTrainer {

  override def apply(data: FeatureVectors): ProbabilisticClassifier = {

    val dtTrainer = new DecisionTreeTrainer(validationPercentage, featuresExaminedPerNode)
    RandomForest(
      data.allOutcomes,
      Range(0, numDecisionTrees) flatMap { _ =>
        dtTrainer(data) match {
          case dt: DecisionTree => Some(dt)
          case _ => None
        }
      }
    )
  }
}
