package org.allenai.nlpstack.parse.poly.ml

import java.io.{File, PrintWriter}

import org.allenai.common.Resource
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.util.Random

/** A CandidatePool is an abstraction for a "pool" of comparable objects.
  *
  * Each object is represented as a feature vector, and is associated with a cost.
  * Lower-cost feature vectors are "better." Usually we want to use a corpus of these "pools"
  * to learn to distinguish "good" feature vectors from "bad" feature vectors.
  *
  * @param scoredCandidates a mapping of feature vectors to costs (lower is better)
  */
case class CandidatePool(scoredCandidates: Map[FeatureVector, Double])

object CandidatePool {
  //implicit val jsFormat = jsonFormat1(CandidatePool.apply)
}

/** A CandidatePoolCorpus is a set of candidate pools (see CandidatePool, above).
  *
  * @param pools the pools in the corpus
  */
case class CandidatePoolCorpus(pools: Iterable[CandidatePool]) {

  private val randomGenerator = new Random

  /** Creates a corpus of "difference vectors" by repeatedly sampling two vectors from a
    * candidate pool and subtracting them. The resulting vector will have a negative cost if
    * the first vector is better (has lower cost) than the second, and a positive cost if the
    * second vector is better (has lower cost) than the first.
    *
    * @param numIters number of samples to generate per pool
    * @return a training data corpus consisting of the resulting difference vectors
    */
  def runSampling(numIters: Int): TrainingData = {
    TrainingData((Range(0, numIters) map { iter =>
      runOneSamplingIteration()
    }).fold(Iterable[(FeatureVector, Double)]())({
      (x: Iterable[(FeatureVector, Double)], y: Iterable[(FeatureVector, Double)]) => x ++ y
    }))
  }

  private def runOneSamplingIteration(): Iterable[(FeatureVector, Double)] = {
    pools filter { pool => pool.scoredCandidates.size >= 2 } map { pool =>
      val orderedCandidates = pool.scoredCandidates.toIndexedSeq
      val (cand1, cand2) = (orderedCandidates(randomGenerator.nextInt(orderedCandidates.size)),
        orderedCandidates(randomGenerator.nextInt(orderedCandidates.size)))
      val differenceVector: FeatureVector = FeatureVector.subtractVectors(cand1._1,
        cand2._1)
      val differenceVectorLabel: Double = cand1._2 - cand2._2
      (differenceVector, differenceVectorLabel)
    }
  }
}

object CandidatePoolCorpus {
  //implicit val jsFormat = jsonFormat1(CandidatePoolCorpus.apply)
}


case class TrainingVectorPool(trainingVectors: Iterable[(FeatureVector, Double)]) {
  override def toString(): String = {
    (trainingVectors map { case (featureVector, cost) =>
      f"${cost}%.2f: $featureVector"
    }).mkString("\n")
  }
}

object TrainingVectorPool {
  implicit val jsFormat = jsonFormat1(TrainingVectorPool.apply)

  /** Writes a stream of training vector pools to a file in JSON format, one per line.
    *
    * @param pools the pools to write
    * @param filename where to write the JSON-serialized pools
    */
  def writeTrainingVectorPools(pools: Iterator[TrainingVectorPool], filename: String): Unit = {
    Resource.using(new PrintWriter(new File(filename))) { writer =>
      for (pool <- pools) writer.println(pool.toJson.compactPrint)
    }
  }
}
