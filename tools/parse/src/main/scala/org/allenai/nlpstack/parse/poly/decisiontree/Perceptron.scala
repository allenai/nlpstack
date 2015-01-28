package org.allenai.nlpstack.parse.poly.decisiontree

// more like just linear model
case class Perceptron(weightsByFeature: Seq[(Int, IndexedSeq[Double])], numOutcomes: Int)
    extends ProbabilisticClassifier {

  weightsByFeature foreach {
    case (feature, weights) =>
      println(s"Feature: $feature, weights: $weights")
  }

  @transient private lazy val weightsByFeatureMap = weightsByFeature.toMap

  // TODO: not currently a distribution
  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    val defaultOutcomes = (Range(0, numOutcomes) map { outcome => (outcome, 0.0) }).toMap
    defaultOutcomes ++ Perceptron.linearCombination(featureVector, weightsByFeatureMap)
  }

  override def allFeatures: Set[Int] = weightsByFeatureMap.keySet
}

object Perceptron {

  def linearCombination(
    featureVector: FeatureVector,
    weightsByFeatureMap: Map[Int, IndexedSeq[Double]]
  ): Map[Int, Double] = {

    val sums: IndexedSeq[Double] = featureVector.nonzeroFeatures map { feat =>
      weightsByFeatureMap.getOrElse(feat, IndexedSeq[Double]())
    } reduce { (x, y) =>
      x.zipAll(y, 0.0, 0.0) map { z => z._1 + z._2 }
    }
    (sums.zipWithIndex map { case (x, y) => (y, x) }).toMap
  }
}

class PerceptronTrainer(numIters: Int, numOutcomes: Int) extends ProbabilisticClassifierTrainer {

  var weightsByFeature: Map[Int, IndexedSeq[Double]] = Map()
  private var totals: Map[(Int, Int), Double] = Map()
  private var timestamps: Map[(Int, Int), Int] = Map()
  private var numInstancesSeen: Int = 0

  override def apply(data: FeatureVectors): ProbabilisticClassifier = {
    weightsByFeature = Map()
    totals = Map()
    timestamps = Map()
    numInstancesSeen = 0
    for (iter <- Range(0, numIters)) {
      println(s"Iter $iter")
      data.getBag.featureVectors foreach { featureVector =>
        updateModel(featureVector)
      }
    }
    // now average the weights
    val averagedWeightsByFeature = weightsByFeature map {
      case (feature, weights) =>
        (feature, weights.zipWithIndex map {
          case (weight, outcome) =>
            val total: Double = totals.getOrElse((feature, outcome), 0.0) +
              (weight * (numInstancesSeen - timestamps.getOrElse((feature, outcome), 0)))
            total / numInstancesSeen // check for divide by zero
        })
    }
    Perceptron(averagedWeightsByFeature.toSeq, numOutcomes)
  }

  private def updateModel(featureVector: FeatureVector): Unit = {
    featureVector.outcome match {
      case Some(truth) =>
        numInstancesSeen += 1
        val guess = predictOutcome(featureVector)
        if (truth != guess) {
          for (feature <- featureVector.nonzeroFeatures) {
            if (!weightsByFeature.contains(feature)) {
              weightsByFeature = weightsByFeature.updated(feature, IndexedSeq.fill(numOutcomes)(0.0))
            }
            updateFeature(feature, truth, 1.0)
            updateFeature(feature, guess, -1.0)
          }
        }
      case None =>
    }
  }

  private def updateFeature(feature: Int, outcome: Int, update: Double): Unit = {
    val currentWeight: Double = weightsByFeature(feature)(outcome)
    totals = totals.updated(
      (feature, outcome),
      currentWeight * (numInstancesSeen - timestamps.getOrElse((feature, outcome), 0))
    )
    timestamps = timestamps.updated((feature, outcome), numInstancesSeen)
    weightsByFeature = weightsByFeature.updated(
      feature,
      weightsByFeature(feature).updated(outcome, currentWeight + update)
    )
    //println(s"Feature: $feature, outcome: $outcome, previous: $currentWeight, new: ${weightsByFeature(feature)(outcome)}")
  }

  private def predictOutcome(featureVector: FeatureVector): Int = {
    val defaultOutcomes = (Range(0, numOutcomes) map { outcome => (outcome, 0.0) }).toMap
    val scoredOutcomes = defaultOutcomes ++
      Perceptron.linearCombination(featureVector, weightsByFeature)
    (scoredOutcomes.toSeq sortBy { _._1 } maxBy { _._2 })._1
  }
}
