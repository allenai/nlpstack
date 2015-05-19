package org.allenai.nlpstack.parse.poly.ml

import java.io.File

import org.allenai.nlpstack.parse.poly.core.Sentence
import reming.DefaultJsonProtocol._
import scala.collection.immutable.HashSet
import scala.io.Source

case class WikiSet(filename: String) {

  @transient val baseSet = {
    val wikiLines = Source.fromFile(new File(filename)).getLines()
    val bloom = new BloomFilter[String](0.05, 11000000)
    var counter = 0
    wikiLines foreach { line =>
      bloom.add(line.toLowerCase)
      counter += 1
      if (counter % 10000 == 0) {
        println(s"Ngrams: Added line $counter")
      }
    }
    bloom
  }

  @transient val prefixes = {
    val wikiLines = Source.fromFile(new File(filename)).getLines()
    val bloom = new BloomFilter[String](0.10, 50000000)
    var counter = 0
    wikiLines foreach { line =>
      val ngram = line.split("_")
      val prefixes = Range(1, ngram.size) map { prefixSize => ngram.take(prefixSize) }
      prefixes foreach { prefix =>
        bloom.add(encodeWords(prefix))
      }
      counter += 1
      if (counter % 10000 == 0) {
        println(s"Prefixes: Added line $counter")
      }
    }
    bloom
  }

  private def encodeWords(words: Seq[String]): String = {
    (words map { _.toLowerCase }).mkString("_")
  }

  def containsPrefix(ngramPrefix: Seq[String]): Boolean = {
    prefixes.contains(encodeWords(ngramPrefix))
  }

  def contains(ngram: Seq[String]): Boolean = {
    baseSet.contains(encodeWords(ngram))
  }

  def identifyNgrams(sentence: Sentence, minLength: Int): Set[(Int, Int)] = {
    val maximalPrefixes = for {
      i <- Range(0, sentence.tokens.size)
    } yield {
      var j = i + 1
      while (j <= sentence.tokens.size &&
        containsPrefix(sentence.tokens.slice(i, j) map { _.word.name })) {
        j += 1
      }
      (i, j)
    }
    (for {
      (i, k) <- maximalPrefixes
      j <- Range(i + minLength, k + 1) if contains(sentence.tokens.slice(i, j) map { _.word.name })
    } yield {
      (i, j)
    }).toSet
  }
}

object WikiSet {
  implicit val jsFormat = jsonFormat1(WikiSet.apply)

}
