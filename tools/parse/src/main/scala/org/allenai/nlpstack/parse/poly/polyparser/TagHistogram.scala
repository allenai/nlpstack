package org.allenai.nlpstack.parse.poly.polyparser

import java.util.Scanner

import org.allenai.nlpstack.parse.poly.core.WordClusters
import org.allenai.nlpstack.parse.poly.ml.FeatureName
import scopt.OptionParser

import scala.io.StdIn

private case class TagHistogramConfig(goldFilenames: String = "", dataSource: String = "")

object TagHistogram {

  /** Command-line for evaluating a set of parses against a gold set.
    *
    * Usage: Evaluate [options]
    *
    * -c <file> | --candidate <file>
    * the file containing the candidate parses (CoNLL-X format)
    * -g <file> | --gold <file>
    * the file containing the gold parses (CoNLL-X format)
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[TagHistogramConfig]("TagHistogram") {
      opt[String]('g', "gold") required () valueName ("<file>") action
        { (x, c) => c.copy(goldFilenames = x) } text ("the (comma-separated files containing" +
          " the gold parses (CoNLL-X format)")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure("unsupported input format")
            }
          }
    }
    val config: TagHistogramConfig = optionParser.parse(args, TagHistogramConfig()).get
    val trainingSource: PolytreeParseSource =
      MultiPolytreeParseSource(config.goldFilenames.split(",") map { path =>
        InMemoryPolytreeParseSource.getParseSource(
          path,
          ConllX(true), config.dataSource
        )
      })

    val qualifyingThreshold = 5
    var wordHistogram = Map[Symbol, Int]()
    for {
      sent <- trainingSource.sentenceIterator
    } {
      sent.tokens foreach { tok =>
        val word = tok.word
        wordHistogram = wordHistogram.updated(
          word,
          1 + wordHistogram.getOrElse(word, 0)
        )
      }
    }
    val commonWords = wordHistogram.toSeq filter {
      case (_, count) =>
        count >= qualifyingThreshold
    } map {
      case (word, _) =>
        word
    }
    println(s"Number of common words: ${commonWords.size}")

    var posWordHistogram = Map[(Symbol, Symbol), Int]()
    var cposWordHistogram = Map[(Symbol, Symbol), Int]()
    for {
      sent <- trainingSource.sentenceIterator
    } {
      sent.tokens foreach { tok =>
        val word = tok.word
        if (commonWords.contains(word)) {
          val pos = tok.getDeterministicProperty('pos)
          val cpos = tok.getDeterministicProperty('cpos)
          posWordHistogram = posWordHistogram.updated(
            (word, pos),
            1 + posWordHistogram.getOrElse((word, pos), 0)
          )
          cposWordHistogram = cposWordHistogram.updated(
            (word, cpos),
            1 + cposWordHistogram.getOrElse((word, cpos), 0)
          )
        }
      }
    }

    val tagHistogram = TagHistogram(commonWords.toSet, posWordHistogram, cposWordHistogram)

    Seq("the", "bank", "pirate", "Shift", "newspaper", "stock", "diet") foreach { word =>
      tagHistogram.getFeatures(Symbol(word)) foreach { feat =>
        println(s"$word: $feat")
      }
    }

    /*
    tagHistogram.words foreach { word =>
      tagHistogram.getFeatures(word) foreach { feat =>
        println(s"$word: $feat")
      }
    }
    */
  }
}

case class TagHistogram(
    commonWords: Set[Symbol],
    posWordHistogram: Map[(Symbol, Symbol), Int],
    cposWordHistogram: Map[(Symbol, Symbol), Int]
) {

  val word2PosHistogram: Map[Symbol, Map[Symbol, Int]] = {
    var tempHistogram = Map[Symbol, Map[Symbol, Int]]()
    for {
      ((word, pos), count) <- posWordHistogram
    } {
      val posHistogram = tempHistogram.getOrElse(word, Map[Symbol, Int]())
      tempHistogram = tempHistogram.updated(word, posHistogram.updated(pos, count))
    }
    tempHistogram
  }

  val word2CposHistogram: Map[Symbol, Seq[(Symbol, Int)]] = {
    var tempHistogram = Map[Symbol, Seq[(Symbol, Int)]]()
    for {
      ((word, cpos), count) <- cposWordHistogram
    } {
      val cposHistogram = tempHistogram.getOrElse(word, Seq[(Symbol, Int)]())
      tempHistogram = tempHistogram.updated(word, (cpos, count) +: cposHistogram)
    }
    tempHistogram
  }

  @transient val words: Iterable[Symbol] = word2CposHistogram.keys

  def getFeatures(word: Symbol): Seq[FeatureName] = {
    if (commonWords.contains(word)) {
      getCommonWordFeatures(word)
    } else if (commonWords.contains(Symbol(word.name.toLowerCase()))) {
      getCommonWordFeatures(Symbol(word.name.toLowerCase)) ++ getRareWordFeatures(word)
    } else {
      getRareWordFeatures(word)
    }
  }

  private def getRareWordFeatures(word: Symbol): Seq[FeatureName] = {
    val wordStr: String = word.name
    val firstLetterCapital = wordStr.headOption match {
      case Some(x) if Character.isUpperCase(x) => Set('firstCap)
      case _ => Set[Symbol]()
    }
    val existsCapital = wordStr match {
      case tokStr: String if tokStr exists Character.isUpperCase => Set('existsCap)
      case _ => Set[Symbol]()
    }
    val allCaps = wordStr match {
      case tokStr: String if tokStr forall Character.isUpperCase => Set('allCaps)
      case _ => Set[Symbol]()
    }
    val existsNumber = wordStr match {
      case tokStr: String if tokStr exists Character.isDigit => Set('existsNum)
      case _ => Set[Symbol]()
    }
    val lexicalProperties: Set[Symbol] =
      firstLetterCapital ++ existsCapital ++ allCaps ++ existsNumber

    val matchingSuffixes = WordClusters.suffixes filter { suffix =>
      wordStr.toLowerCase.endsWith(suffix.name.toLowerCase)
    }

    Seq(FeatureName(Seq('rare))) ++
      (lexicalProperties.toSeq map { x => FeatureName(Seq(x)) }) ++
      (matchingSuffixes.toSeq map { suffix => FeatureName(Seq('suffix, suffix)) })
  }

  private def getCommonWordFeatures(word: Symbol): Seq[FeatureName] = {
    val cposHistogram = word2CposHistogram.getOrElse(word, Seq[(Symbol, Int)]())
    val total = (cposHistogram map { _._2 }).sum
    val cposFeatures = cposHistogram flatMap {
      case (cpos, count) =>
        Seq(
          if (count.toFloat / total > 0.5) {
            Some(FeatureName(Seq('gr50cpos, cpos)))
          } else {
            None
          },
          if (count.toFloat / total > 0.1) {
            Some(FeatureName(Seq('gr10cpos, cpos)))
          } else {
            None
          },
          if (count.toFloat / total > 0.02) {
            Some(FeatureName(Seq('gr2cpos, cpos)))
          } else {
            None
          }
        ).flatten
    }
    val posHistogram = word2PosHistogram.getOrElse(word, Seq[(Symbol, Int)]())
    val posTotal = (posHistogram map { _._2 }).sum
    val posFeatures = posHistogram flatMap {
      case (pos, count) =>
        Seq(
          if (count.toFloat / posTotal > 0.5) {
            Some(FeatureName(Seq('gr50pos, pos)))
          } else {
            None
          },
          if (count.toFloat / posTotal > 0.1) {
            Some(FeatureName(Seq('gr10pos, pos)))
          } else {
            None
          },
          if (count.toFloat / posTotal > 0.02) {
            Some(FeatureName(Seq('gr2pos, pos)))
          } else {
            None
          }
        ).flatten
    }
    (posFeatures ++ cposFeatures).toSeq
  }
}
