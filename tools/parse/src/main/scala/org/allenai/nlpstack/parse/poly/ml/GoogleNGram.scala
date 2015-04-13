package org.allenai.nlpstack.parse.poly.ml

import org.allenai.datastore._

import java.io._

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
case class GoogleNGram() {

  val ngramLoc = "/Users/sumithrab/Downloads/google-ngrams/nodes"
  val ngramDir = new File(ngramLoc)

  //val ngramMap = GoogleNGram.constructNgramTable(ngramDir)

  //val ngramMapFile = "/Users/sumithrab/Downloads/google-ngrams/node-map.txt"
  //val writer = new PrintWriter(new File(ngramMapFile))
  //val sortedKeys = ngramMap.keySet.toSeq.sorted
  //writer.write(s"Total no. of keys: ${sortedKeys.length}\n")
  //for (k <- sortedKeys) {
  //  writer.write(k + "\n")
  //  for (x <- ngramMap(k)) {
  //    writer.write(x.syntacticNgrams.mkString(" ") + "\t" + x.frequency + "\n")
  //  }
  //  writer.write("\n")
  //}
  //writer.close
  val ngramFreqs = "/Users/sumithrab/Downloads/google-ngrams/uniquenodes-freq1000.txt"
  GoogleNGram.getNgramFrequencies(ngramDir, ngramFreqs)
}

case class SyntacticNgram(word: String, posTag: String, depLabel: String, headIndex: Int)
case class NgramInfo(syntacticNgrams: Seq[SyntacticNgram], frequency: Long)

object GoogleNGram {

  val freqCutoff: Int = 1000

  def createSyntacticNgram(syntacticNgramsStr: String): Seq[SyntacticNgram] = {
    val syntacticNgramStrs = syntacticNgramsStr.split(" ").map(x => x.trim).toSeq
    for {
      syntacticNgramStr <- syntacticNgramStrs
      syntacticNgram <- getSyntacticNgram(syntacticNgramStr)
    } yield {
      syntacticNgram
    }
  }

  def getSyntacticNgram(syntacticNgramStr: String): Option[SyntacticNgram] = {
    syntacticNgramStr.split("/").map(x => x.trim) match {
      case Array(word: String, posTag: String, depLabel: String, headIxStr: String) =>
        Some(new SyntacticNgram(word, posTag, depLabel, headIxStr.toInt))
      case _ => None
    }
  }

  def parseLine(line: String): (String, Seq[SyntacticNgram], Long) = {
    line.split("\t").map(x => x.trim) match {
      case Array(ngram: String, syntacticNgramStr: String, freqStr: String, _*) =>
        (ngram, createSyntacticNgram(syntacticNgramStr), freqStr.toLong)
    }
  }

  def constructNgramTable(ngramDir: File): Map[String, Seq[NgramInfo]] = {
    val table = scala.collection.mutable.HashMap.empty[String, Seq[NgramInfo]]
    for {
      file <- ngramDir.listFiles
      line <- Source.fromFile(file).getLines
    } yield {
      val ngramDetails = parseLine(line)
      val alphaNumPattern = """[a-zA-Z]+"""
      if (ngramDetails._1.matches(alphaNumPattern) && (ngramDetails._3 > freqCutoff)) {
        table(ngramDetails._1) = table.getOrElse(ngramDetails._1, Seq.empty) :+
          new NgramInfo(ngramDetails._2, ngramDetails._3)
      }
    }
    table.mapValues(x => x.sortBy(_.frequency).reverse).toMap
  }

  def getNgramFrequencies(ngramDir: File, ngramFreqs: String): Unit = {
    // Open Writer to output frequencies of the ngrams
    val opWriter = new PrintWriter(ngramFreqs)
    var prevToken = ""
    var numTokens = 0
    for {
      file <- ngramDir.listFiles
      line <- Source.fromFile(file).getLines
    } yield {
      val ngramDetails = parseLine(line)
      val alphaNumPattern = """[a-zA-Z]+"""
      if (ngramDetails._1.matches(alphaNumPattern) && (ngramDetails._3 > freqCutoff) &&
        !ngramDetails._1.equalsIgnoreCase(prevToken)) {
        opWriter.write(
          ngramDetails._1 + "\t" /*+ ngramDetails._2.mkString(" ")  + "\t"*/ +
            ngramDetails._3 + "\n"
        )
        prevToken = ngramDetails._1
        numTokens += 1
      }
    }
    opWriter.println(s"Total no. of tokens: ${numTokens}")
    opWriter.close
  }
}