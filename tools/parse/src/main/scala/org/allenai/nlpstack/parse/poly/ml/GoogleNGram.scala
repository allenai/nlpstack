package org.allenai.nlpstack.parse.poly.ml

import org.allenai.datastore._
import org.allenai.nlpstack.core.PostaggedToken

import java.io._

import reming.DefaultJsonProtocol._

import scala.io._

/** A class that parses Google N-Gram data
  * (http://commondatastorage.googleapis.com/books/syntactic-ngrams/index.html) to provide
  * information about a requested n-gram.
  * Takes the datastore location details for a data directory and parses each file, expected
  * to be in the following format
  * (from https://docs.google.com/document/d/14PWeoTkrnKk9H8_7CfVbdvuoFZ7jYivNTkBX2Hj7qLw/edit) -
  *   head_word<TAB>syntactic-ngram<TAB>total_count<TAB>counts_by_year
  * The counts_by_year format is a tab-separated list of year<comma>count items.
  * Years are sorted in ascending order, and only years with non-zero counts are included.
  * The syntactic-ngram format is a space-separated list of tokens, each token format is:
  *   “word/pos-tag/dep-label/head-index”.
  * The word field can contain any non-whitespace character.
  * The other fields can contain any non-whitespace character except for ‘/’.
  *   pos-tag is a Penn-Treebank part-of-speech tag.
  *   dep-label is a stanford-basic-dependencies label.
  *   head-index is an integer, pointing to the head of the current token.
  *   “1” refers to the first token in the list, 2 the second,
  *      and 0 indicates that the head is the root of the fragment.
  */
case class GoogleNGram(
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

/** Utility case classes to represent information associated with an ngram in the Google Ngram
  * corpus.
  */
case class SyntacticInfo(word: String, posTag: String, depLabel: String, headIndex: Int)
case class NgramInfo(syntacticNgram: Seq[SyntacticInfo], frequency: Long)

/** Companion object. Contains methods to parse a Google Ngram corpus. This is not specific to
  * the type of corpus, i.e. whether unigram, bigram, etc.
  */
object GoogleNGram {

  implicit val jsonFormat = jsonFormat4(GoogleNGram.apply)

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

  /** Looks up specified ngramMap for the given token and returns a map of the frequency for each
    * dependency label for the given token word and POS, normalized over the total frequency for all
    * possible dependency labels.
    * @param token the token to look up
    * @param ngramMap the table mapping a word to the sequence of NgramInfos, as obtained from the
    * GoogleNGram class object
    * @param frequencyCutoff the frequency cutoff that was used to construct the map. This is used
    * here to shift the scale of the frequencies to start from the cutoff point instead of 1.
    */
  def getDepLabelNormalizedDistribution(
    token: PostaggedToken, ngramMap: Map[String, Seq[NgramInfo]], frequencyCutoff: Int
  ): Map[String, Double] = {
    val tokenNodeInfos = (for {
      tokNgrams <- ngramMap.get(token.string.toLowerCase)
    } yield {
      getTokenUnigramInfo(
        Option(token.postag), tokNgrams, frequencyCutoff
      )
    }).getOrElse(Seq.empty[UnigramInfo])

    // Get the total frequency for all nodes aggregated above for the current token to
    // normalize them.
    val totalFrequency = tokenNodeInfos.foldLeft(0L)((a, b) => a + b.frequency)

    // Iterate over all the nodes, and normalize frequencies by the total frequency.
    (for {
      tokenNodeInfo <- tokenNodeInfos
    } yield {
      val normalizedFrequency = tokenNodeInfo.frequency.toDouble / totalFrequency
      (tokenNodeInfo.syntacticUnigram.depLabel -> normalizedFrequency)
    }).toMap
  }

  /** Looks up specified ngramMap for the given word and returns a map of the frequency for each
    * POS tag for the given word, normalized over the total frequency for all possible POS tags.
    * @param word the word to look up
    * @param ngramMap the table mapping a word to the sequence of NgramInfos, as obtained from the
    * GoogleNGram class object
    * @param frequencyCutoff the frequency cutoff that was used to construct the map. This is used
    * here to shift the scale of the frequencies to start from the cutoff point instead of 1.
    */
  def getPosTagNormalizedDistribution(
    word: String, ngramMap: Map[String, Seq[NgramInfo]], frequencyCutoff: Int
  ): Map[String, Double] = {
    val tokenNodeInfos = (for {
      tokNgrams <- ngramMap.get(word.toLowerCase)
    } yield {
      getTokenUnigramInfo(
        None, tokNgrams, frequencyCutoff
      )
    }).getOrElse(Seq.empty[UnigramInfo])

    // Get the total frequency for all nodes aggregated above for the current token to
    // normalize them.
    val totalFrequency = tokenNodeInfos.foldLeft(0L)((a, b) => a + b.frequency)

    val groupedTokenNodes = tokenNodeInfos.groupBy(_.syntacticUnigram.posTag)

    // Iterate over all keys if the groupedTokenNodes map (the POS tags) and normalize frequencies
    // by the total frequency.
    (for {
      posTag <- groupedTokenNodes.keys
    } yield {
      val totalFrequencyThisPostag =
        groupedTokenNodes(posTag).map(x => x.frequency).foldLeft(0L)((a, b) => a + b)
      val normalizedFrequency = totalFrequencyThisPostag.toDouble / totalFrequency
      (posTag, normalizedFrequency)
    }).toMap
  }

  /** Helper Method. Takes a token's POS tag and a seq of NgramInfos associated with the token
    * and returns a seq of UnigramInfos, filtered to just the ones that are relevant to the
    * token POS tag if specified.
    * For unigrams, we expect just one SyntacticNgram per NgramInfo.
    * E.g: NgramInfo entries for the word "a" look like below:
    * SyntacticNgram(a,DT,dep,0) 14737935
    * SyntacticNgram(a,NNP,nn,0)  4184390
    * SyntacticNgram(a,NNP,dep,0) 2070101
    * SyntacticNgram(a,DT,quantmod,0) 2069740
    * SyntacticNgram(a,DT,ROOT,0) 1368856
    * where each NgramInfo (one per line) has only one SyntacticNgram. Here we try to aggregate
    * the SyntacticNgrams that match the POS tag of the current token and get the frequency
    * distribution of the different possible dependency labels.
    */
  private def getTokenUnigramInfo(
    posTag: Option[String], ngramInfos: Seq[NgramInfo], frequencyCutoff: Int
  ): Seq[UnigramInfo] = {
    val ngramInfosFiltered = posTag match {
      case Some(x: String) =>
        ngramInfos.filter(ngramInfo => ngramInfo.syntacticNgram.head.posTag.equalsIgnoreCase(x))
      case _ => ngramInfos
    }
    ngramInfosFiltered map {
      ngramInfoForThisTok =>
        // Scale down the frequencies so that the cutoff frequency (minimum) is treated as the
        // starting point (frequency 1).
        new UnigramInfo(
          ngramInfoForThisTok.syntacticNgram.head,
          ngramInfoForThisTok.frequency - frequencyCutoff
        )
    }
  }
}
