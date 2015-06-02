package org.allenai.nlpstack.parse.poly.ml

import java.io.File

import org.allenai.nlpstack.parse.poly.core.{ SentenceTagger, TokenTag, TaggedSentence, Sentence }
import reming.DefaultJsonProtocol._
import scala.collection.immutable.HashSet
import scala.io.Source

case class WikiSet(filename: String) {

  private def getFilteredWikiEntries: Iterator[String] = {
    Source.fromFile(new File(filename)).getLines() filter { line =>
      val filteredTokens = line.split("_") filter { token =>
        token.size > 0 && !(token.head == '(' && token.last == ')')
      }
      filteredTokens.size > 1
    }
  }

  @transient val baseSet = {
    val bloom = new BloomFilter[String](0.05, 10000000)
    var counter = 0
    getFilteredWikiEntries foreach { line =>
      bloom.add(line.toLowerCase)
      counter += 1
      if (counter % 10000 == 0) {
        println(s"Ngrams: Added line $counter")
      }
    }
    bloom
  }

  @transient val prefixes = {
    val bloom = new BloomFilter[String](0.10, 21000000)
    var counter = 0
    getFilteredWikiEntries foreach { line =>
      val ngram = line.split("_")
      val prefixes = Range(1, ngram.size) map { prefixSize => ngram.take(prefixSize) }
      prefixes foreach { prefix =>
        bloom.add(encodeWords(prefix))
        counter += 1
      }
      if (counter % 10000 == 0) {
        println(s"Prefixes: Added prefix $counter")
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

case class WikiSetTagger(wikiset: WikiSet) extends SentenceTagger {

  override def tag(sentence: Sentence): TaggedSentence = {
    val ngrams: Set[(Int, Int)] = wikiset.identifyNgrams(sentence, 2)
    val ngramBeginnings = ngrams map { _._1 }
    val ngramEndings = ngrams map { _._2 }
    val ngramInternals: Set[Int] = ngrams flatMap { case (start, finish) => Range(start + 1, finish) }

    TaggedSentence(
      sentence,
      (Range(1, sentence.tokens.size) map { tokIndex =>
      (
        tokIndex,
        Set(
          if (ngramBeginnings.contains(tokIndex)) { Some(TokenTag('wiki, 'B)) } else { None },
          if (ngramEndings.contains(tokIndex)) { Some(TokenTag('wiki, 'E)) } else { None },
          if (ngramInternals.contains(tokIndex)) { Some(TokenTag('wiki, 'I)) } else { None }
        ).flatten
      )
    }).toMap
    )
  }
}
