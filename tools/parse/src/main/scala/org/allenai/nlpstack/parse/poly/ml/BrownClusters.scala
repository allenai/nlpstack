package org.allenai.nlpstack.parse.poly.ml

import reming.DefaultJsonProtocol._

import scala.io.Source

case class BrownClusters(clusters: Iterable[(Symbol, Seq[Int])]) {
  require(!(clusters flatMap { cluster => cluster._2 }).toSet.contains(0))

  @transient
  private val unkCluster = Symbol("0")

  @transient
  private val clusterMap: Map[Symbol, Seq[Int]] =
    clusters.toMap mapValues { clusters => clusters.sorted }

  @transient
  private val mostSpecificClusterMap: Map[Symbol, Symbol] =
    clusterMap mapValues { clusters => Symbol(clusters.last.toString) }

  def getMostSpecificCluster(word: Symbol): Symbol = {
    mostSpecificClusterMap.getOrElse(word, unkCluster)
  }

  def getKthCluster(word: Symbol, k: Int): Symbol = {
    val wordClusters = getAllClusters(word)
    wordClusters.lift(k).getOrElse(wordClusters.last)
  }

  // It is guaranteed that the return value will be a non-empty sequence.
  def getAllClusters(word: Symbol): Seq[Symbol] = {
    clusterMap.get(word) match {
      case None => Seq(unkCluster)
      case Some(clusters) if clusters.isEmpty => Seq(unkCluster)
      case Some(clusters) => clusters map { cluster => Symbol(cluster.toString) }
    }
  }
}

object BrownClusters {
  implicit val brownClustersFormat = jsonFormat1(BrownClusters.apply)

  def fromLiangFormat(filename: String): BrownClusters = {
    val fileContents: Map[String, (String, Int)] =
      (Source.fromFile(filename).getLines map { line =>
        val fields: Array[String] = line.split("\t")
        require(fields.size == 3, "Liang's Brown cluster format must have three columns per line.")
        (fields(1), (fields(0), fields(2).toInt))
      }).toMap
    val wordsToBitstrings = fileContents mapValues { case (bitstring, _) => bitstring }
    val wordsToFrequency = fileContents mapValues { case (_, freq) => freq }
    fromStringMap(wordsToBitstrings, wordsToFrequency)
  }

  def fromStringMap(
    wordsToBitstrings: Map[String, String],
    wordsToFrequency: Map[String, Int]
  ): BrownClusters = {

    val allBitstringPrefixes: Set[String] =
      (wordsToBitstrings.values.toSet.toSeq flatMap { bitstring: String =>
        (0 to bitstring.size) map { x =>
          bitstring.take(x)
        }
      }).toSet
    val bitstringEncoding: Map[String, Int] =
      (allBitstringPrefixes.toSeq sortBy { bitstring =>
        bitstring.size
      }).zipWithIndex.toMap
    val wordsToEncodings: Map[Symbol, Seq[Int]] =
      wordsToBitstrings map {
        case (word, bitstring) =>
          (Symbol(word),
            (0 to bitstring.size) map { x =>
              1 + bitstringEncoding(bitstring.take(x))
            })
      }
    BrownClusters(wordsToEncodings.toSeq)
  }
}
