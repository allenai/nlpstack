package org.allenai.nlpstack.parse.poly.ml

import org.allenai.datastore._
import org.allenai.nlpstack.core.PostaggedToken

import java.io._

import org.allenai.nlpstack.parse.poly.core._
import reming.DefaultJsonProtocol._

import scala.io._

/** format: OFF
  * A class that parses Google N-Gram data
  * (http://commondatastorage.googleapis.com/books/syntactic-ngrams/index.html) to provide
  * information about a requested n-gram.
  * Takes the datastore location details for a data directory and parses each file, expected
  * to be in the following format
  * (from https://docs.google.com/document/d/14PWeoTkrnKk9H8_7CfVbdvuoFZ7jYivNTkBX2Hj7qLw/edit) -
  *   head_word<TAB>syntactic-ngram<TAB>total_count<TAB>counts_by_year
  * The counts_by_year format is a tab-separated list of year<comma>count items.
  * Years are sorted in ascending order, and only years with non-zero counts are included.
  * The syntactic-ngram format is a space-separated list of tokens, each token format is:
  * “word/pos-tag/dep-label/head-index”.
  * The word field can contain any non-whitespace character.
  * The other fields can contain any non-whitespace character except for ‘/’.
  *   pos-tag is a Penn-Treebank part-of-speech tag.
  *   dep-label is a stanford-basic-dependencies label.
  *   head-index is an integer, pointing to the head of the current token.
  *   “1” refers to the first token in the list, 2 the second,
  *      and 0 indicates that the head is the root of the fragment.
  * format: ON
  */
case class DatastoreGoogleNGram(
    groupName: String, artifactName: String, version: Int, frequencyCutoff: Int
) {

  @transient val googleNgramPath = Datastore.directoryPath(
    groupName,
    artifactName,
    version
  )

  @transient val googleNgramDir = new File(googleNgramPath.toString)
  @transient val ngramMap = GoogleNGram.constructNgramTable(googleNgramDir, frequencyCutoff)
}

/** Companion object.
  */
object DatastoreGoogleNGram {
  implicit val jsonFormat = jsonFormat4(DatastoreGoogleNGram.apply)
}

/** Utility case classes to represent information associated with an ngram in the Google Ngram
  * corpus.
  */
case class SyntacticInfo(word: String, posTag: String, depLabel: String, headIndex: Int)
case class NgramInfo(syntacticNgram: Seq[SyntacticInfo], frequency: Long)

/** Object containing utility methods to parse a Google Ngram corpus. This is not specific to
  * the type of corpus, i.e. whether unigram, bigram, etc.
  */
object GoogleNGram {

  /** Constructs a table to map a word to the sequence of different ngrams associated with it
    * with associated info for each ngram.
    */
  def constructNgramTable(ngramDir: File, frequencyCutoff: Int): Map[String, Seq[NgramInfo]] = {
    val table = scala.collection.mutable.HashMap.empty[String, Seq[NgramInfo]]
    for {
      file <- ngramDir.listFiles
      line <- Source.fromFile(file).getLines
    } yield {
      val ngramDetails = parseLine(line)
      val alphaNumPattern = """[a-zA-Z]+"""
      if (ngramDetails._1.matches(alphaNumPattern) && (ngramDetails._3 > frequencyCutoff)) {
        table(ngramDetails._1) = table.getOrElse(ngramDetails._1, Seq.empty) :+
          new NgramInfo(ngramDetails._2, ngramDetails._3)
      }
    }
    table.mapValues(x => x.sortBy(_.frequency).reverse).toMap
  }

  /** Helper Method. Takes a string of the following format:
    * shepherd/NNP/nn/2 woods/NNS/pobj/0
    * which can contain multiple syntactic n-grams, and generates a seq of SyntacticInfo objects,
    * one for each word in the ngram, to create the NgramInfo from.
    */
  private def createSyntacticNgram(syntacticNgramsStr: String): Seq[SyntacticInfo] = {
    val syntacticNgramStrs = syntacticNgramsStr.split(" ").map(x => x.trim).toSeq
    for {
      syntacticNgramStr <- syntacticNgramStrs
      syntacticNgram <- getSyntacticInfo(syntacticNgramStr)
    } yield {
      syntacticNgram
    }
  }

  /** Helper Method. Takes a string representing a single syntactic ngram and creates a
    * SyntacticNgram object from it.
    */
  private def getSyntacticInfo(syntacticNgramStr: String): Option[SyntacticInfo] = {
    syntacticNgramStr.split("/").map(x => x.trim) match {
      case Array(word: String, posTag: String, depLabel: String, headIxStr: String) =>
        Some(new SyntacticInfo(word, posTag, depLabel, headIxStr.toInt))
      case _ => None
    }
  }

  /** Helper Method. Breaks a tab-separated line with below format:
    * woods shepherd/NNP/nn/2 woods/NNS/pobj/0  11 1895,1  1899,1  1923,1  1933,3
    * and creates a tuple containing the word the line is about, the sequence of SyntacticInfo
    * objects for the words in each ngram associated with the word, and the total frequency for
    * each ngram. Ignores the last field (year-wise frequency breakdown) in the tab-delimited line.
    */
  private def parseLine(line: String): (String, Seq[SyntacticInfo], Long) = {
    line.split("\t").map(x => x.trim) match {
      case Array(ngram: String, syntacticNgramStr: String, freqStr: String, _*) =>
        (ngram.toLowerCase, createSyntacticNgram(syntacticNgramStr), freqStr.toLong)
    }
  }
}

/** Encapsulates unigram info pertaining to a word. Instead of a seq of SyntacticInfo objects in the
  * general purpose NgramInfo class, here we have a single SyntacticInfo representing the info for
  * a single gram.
  */
case class UnigramInfo(syntacticUnigram: SyntacticInfo, frequency: Long)

/** Object encapsulating some functionality specific to unigrams. Used wherever features need to be
  * constructed based on unigrams (Google Ngram Nodes).
  */
object GoogleUnigram {

  def getNormalizedTagDistribution(
    token: String, ngramMap: Map[String, Seq[NgramInfo]], frequencyCutoff: Int, coarsen: Boolean
  ): Map[String, Double] = {

    def normalizeHistogram(histogram: Map[String, Long]): Map[String, Double] = {
      val normalizer: Float = histogram.values.sum
      require(normalizer > 0d)
      histogram mapValues { _ / normalizer }
    }
    val maybeNgramInfos: Option[Seq[NgramInfo]] = ngramMap.get(token.toLowerCase)
    val maybeTagHistogram: Option[Map[String, Long]] = maybeNgramInfos map { ngramInfos =>
      ngramInfos map { ngramInfo =>
        (
          if(coarsen) {
            WordClusters.ptbToUniversalPosTag(ngramInfo.syntacticNgram.head.posTag)
          } else {
            ngramInfo.syntacticNgram.head.posTag
          },
          ngramInfo.frequency
        )
      } groupBy {
        case (tag, _) =>
          tag
      } mapValues { x =>
        (x map { _._2 }).sum
      }
    }
    maybeTagHistogram match {
      case Some(tagHistogram) =>
        normalizeHistogram(tagHistogram)
      case None => Map[String, Double]()
    }
  }

  /** Method to bucketize a given normalized frequency for feature generation.
    */
  def getFrequencyBucketForFeature(frequency: Double) : String = {
    if (frequency <= 0.05) {
      "Freq1to5"
    } else if (frequency <= 0.20) {
      "Freq6to20"
    } else if (frequency <= 0.50) {
      "Freq21to50"
    } else if (frequency <= 0.95) {
      "Freq51to95"
    } else {
      "Freq96to100"
    }
  }
}

sealed trait GoogleUnigramTagType {
  val name: String
}
case object GoogleUnigramPos extends GoogleUnigramTagType {
  val name: String = "posTag"
}
case object GoogleUnigramCpos extends GoogleUnigramTagType {
  val name: String = "cposTag"
}

object GoogleUnigramTagType {
  private implicit val googleUnigramPosFormat = jsonFormat0(() => GoogleUnigramPos)
  private implicit val googleUnigramCposFormat = jsonFormat0(() => GoogleUnigramCpos)

  implicit val unigramTagTypeJsonFormat = parentFormat[GoogleUnigramTagType](
    childFormat[GoogleUnigramPos.type, GoogleUnigramTagType],
    childFormat[GoogleUnigramCpos.type, GoogleUnigramTagType]
  )
}


case class GoogleUnigramTagger(
  googleNgram: DatastoreGoogleNGram,
  tagType: GoogleUnigramTagType
) extends TokenTagger {

  override def tag(tok: Token): Set[TokenTag] = {

    val tagFreqMap: Map[String, Double] = tagType match {
      case GoogleUnigramPos =>
        GoogleUnigram.getNormalizedTagDistribution(
          tok.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff, coarsen = false
        )
      case GoogleUnigramCpos =>
        GoogleUnigram.getNormalizedTagDistribution(
          tok.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff, coarsen = true
        )
    }
    // Create feature for each dependency label based on the normalized frequency
    // bucket it lies in.
    val frequencyFeatureMap: Set[TokenTag] = for {
      tag <- tagFreqMap.keySet
    } yield {
      val normalizedFrequency = tagFreqMap(tag)
      val freqBucket =
        tagType.name + GoogleUnigram.getFrequencyBucketForFeature(normalizedFrequency)
      TokenTag(Symbol(freqBucket), Symbol(tag))
    }
    val bestTagMapping: Set[TokenTag] =
      if (tagFreqMap.nonEmpty) {
        val mostLikelyTag = (tagFreqMap maxBy {
          _._2
        })._1
        Set(TokenTag(Symbol(s"${tagType.name}MostLikely"), Symbol(mostLikelyTag)))
      } else {
        Set()
      }
    frequencyFeatureMap ++ bestTagMapping
  }
}
