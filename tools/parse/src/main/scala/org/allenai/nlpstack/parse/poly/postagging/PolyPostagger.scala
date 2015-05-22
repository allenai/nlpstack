package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.{ SentenceSource, Sentence }
import org.allenai.nlpstack.parse.poly.eval.{ TaggingEvaluator, CposSentAccuracy, EvaluationStatistic }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.parse.poly.polyparser.{ FileBasedPolytreeParseSource, PolytreeParseSource, PolytreeParseFileFormat }
import org.allenai.nlpstack.postag._
import reming.DefaultJsonProtocol._

import scala.compat.Platform
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object PolyPostagger {

  def fullTaggingEvaluation(tagger: SentenceTagger, testFiles: String,
    testFileFormat: PolytreeParseFileFormat, dataSource: String,
    oracleNbestSize: Int): Unit = {

    val testSources: Map[String, PolytreeParseSource] =
      (testFiles.split(",") map { path =>
        (path, FileBasedPolytreeParseSource.getParseSource(
          path,
          testFileFormat, dataSource
        ))
      }).toMap
    for ((sourcePath, testSource) <- testSources) {
      println(s"Checking tagging accuracy on test set ${sourcePath}.")
      evaluateTaggerOnTestSet(tagger, testSource)
    }
  }

  def tagTestSet(
    tagger: SentenceTagger,
    sentenceSource: SentenceSource
  ): Iterator[TaggedSentence] = {

    import scala.concurrent.ExecutionContext.Implicits.global
    val taggingTasks: Iterator[Future[TaggedSentence]] =
      for {
        sentence <- sentenceSource.sentenceIterator
      } yield Future {
        tagger.tag(sentence)
      }
    val futureTagged: Future[Iterator[TaggedSentence]] = Future.sequence(taggingTasks)
    Await.result(futureTagged, 2 days)
  }

  def evaluateTaggerOnTestSet(tagger: SentenceTagger, parseSource: PolytreeParseSource): Unit = {
    println("Tagging test set.")
    val startTime: Long = Platform.currentTime
    val candidateTaggedSentences = tagTestSet(tagger, parseSource)
    val stats: Seq[EvaluationStatistic] = Seq(
      CposSentAccuracy(verbose = false)
    )
    stats foreach { stat => stat.reset() }

    val goldTaggedSentences = for {
      goldSentence <- parseSource.sentenceIterator
    } yield {
      TaggedSentence(
        goldSentence,
        (goldSentence.tokens.zipWithIndex map {
        case (tok, index) =>
          (index, tok.getProperty('cpos) map { prop => TokenTag('autoCpos, prop) })
      }).toMap
      )
    }

    TaggingEvaluator.evaluate(candidateTaggedSentences, goldTaggedSentences, stats)
    val parsingDurationInSeconds: Double = (Platform.currentTime - startTime) / 1000.0
    val numParses = parseSource.parseIterator.size
    println("Parsed %d sentences in %.1f seconds, an average of %.1f sentences per second.".format(
      numParses, parsingDurationInSeconds,
      numParses.toDouble / parsingDurationInSeconds
    ))
  }

}

