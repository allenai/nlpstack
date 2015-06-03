package org.allenai.nlpstack.parse.poly.ml

import org.allenai.nlpstack.parse.poly.core.{ TokenTag, TokenTagger, Token }
import reming.DefaultJsonProtocol._

import scala.io.Source

case class BrownClusters(allClusters: Iterable[(Symbol, Seq[Int])]) {
  require(
    !(allClusters flatMap { cluster => cluster._2 }).toSet.contains(0),
    s"You've pre-assigned cluster 0 to a word. This cluster is reserved."
  )

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

  @transient
  private val unkCluster = Symbol("0")

  @transient
  private val clusterMap: Map[Symbol, Seq[Int]] =
    allClusters.toMap mapValues { clusters => clusters.sorted }

  @transient
  private val mostSpecificClusterMap: Map[Symbol, Symbol] =
    clusterMap mapValues { clusters => Symbol(clusters.last.toString) }

}

object BrownClusters {
  implicit val brownClustersFormat = jsonFormat1(BrownClusters.apply)

  /** Reads a trained set of Brown clusters from a file in Percy Liang's format.
    *
    * Each line of this file corresponds to a token, and has three tab-separated fields:
    *
    * CLUSTER <tab> TOKEN <tab> COUNT
    *
    * where CLUSTER is the bitstring representation of TOKEN's cluster, and COUNT is the token's
    * count in the source corpus (from which the Brown clusters were trained)
    *
    * @param filename name of file in Liang format
    * @return a BrownClusters object
    */
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

  /** Initializes a BrownClusters object from two maps, one that maps words to their Brown cluster
    * (bitstring representation), another that maps words to their frequency in the source corpus
    * (from which the Brown clusters were trained).
    *
    * @param wordsToBitstrings maps words to the bitstring representation of their Brown cluster
    * @param wordsToFrequency maps words to their frequency in the source corpus
    * @return a BrownClusters object
    */
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

/** The BrownClustersTagger tags the tokens of a sentence with their Brown clusters. */
case class BrownClustersTagger(clusters: Seq[BrownClusters]) extends TokenTagger {

  def tag(token: Token): Set[TokenTag] = {
    val tokStr = token.word.name.toLowerCase
    (for {
      (cluster, clusterId) <- clusters.zipWithIndex
    } yield {
      Seq(TokenTag(
        Symbol(s"brown$clusterId"),
        cluster.getMostSpecificCluster(Symbol(tokStr))
      ))
    }).flatten.toSet
  }
}
