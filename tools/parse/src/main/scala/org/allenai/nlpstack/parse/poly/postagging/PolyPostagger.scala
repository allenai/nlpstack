package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.{ SentenceSource, TaggedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.eval.{ TaggingEvaluator, CposSentAccuracy, EvaluationStatistic }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.parse.poly.polyparser.{ FileBasedPolytreeParseSource, PolytreeParseSource, PolytreeParseFileFormat }
import org.allenai.nlpstack.postag._
import reming.DefaultJsonProtocol._

import scala.compat.Platform
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

/** Interface for a part-of-speech tagger. */
trait PolyPostagger {

  /** Tags the tokens of a sentence with its part-of-speech tags.
    *
    * @param sentence the sentence you want to tag
    * @param constraints user-specified constraints on the tags
    * @return the tagged sentence
    */
  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence]
}

object PolyPostagger {

  /** Initializes a part-of-speech tagger.
    *
    * @param initializer the initialization recipe
    * @return the initialized tagger
    */
  def initializePostagger(initializer: PolyPostaggerInitializer): PolyPostagger = {
    initializer match {
      case FactoriePostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(defaultPostagger, useCoarseTags)
      case StanfordPostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(new StanfordPostagger(), useCoarseTags)
      case SimplePostaggerInitializer(configFile) =>
        SimplePostagger.load(configFile)
      case SwappablePostaggerInitializer(configFile, useCoarseTags) =>
        //NLPStackPostagger(new StanfordPostagger(), useCoarseTags)
        NLPStackPostagger(defaultPostagger, useCoarseTags)
    }
  }

  def fullTaggingEvaluation(tagger: PolyPostagger, testFiles: String,
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
    tagger: PolyPostagger,
    sentenceSource: SentenceSource
  ): Iterator[Option[TaggedSentence]] = {

    import scala.concurrent.ExecutionContext.Implicits.global
    val taggingTasks: Iterator[Future[Option[TaggedSentence]]] =
      for {
        sentence <- sentenceSource.sentenceIterator
      } yield Future {
        tagger.tag(sentence)
      }
    val futureTagged: Future[Iterator[Option[TaggedSentence]]] = Future.sequence(taggingTasks)
    Await.result(futureTagged, 2 days)
  }

  def evaluateTaggerOnTestSet(tagger: PolyPostagger, parseSource: PolytreeParseSource): Unit = {
    println("Tagging test set.")
    val startTime: Long = Platform.currentTime
    val candidateTaggedSentences = tagTestSet(tagger, parseSource).flatten
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
          (index, tok.getProperty('cpos))
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

/** Recipe for initializing a part-of-speech tagger. */
sealed trait PolyPostaggerInitializer

/** Initializes a default Factorie part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class FactoriePostaggerInitializer(useCoarseTags: Boolean) extends PolyPostaggerInitializer

/** Initializes a default Stanford part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class StanfordPostaggerInitializer(useCoarseTags: Boolean) extends PolyPostaggerInitializer

/** Initializes a SimplePostagger.
  *
  * @param configFile filename containing the JSON configuration
  */
case class SimplePostaggerInitializer(configFile: String) extends PolyPostaggerInitializer

case class SwappablePostaggerInitializer(configFile: String, useCoarseTags: Boolean)
  extends PolyPostaggerInitializer

object PolyPostaggerInitializer {
  private implicit val factorieInitFormat = jsonFormat1(FactoriePostaggerInitializer.apply)
  private implicit val stanfordInitFormat = jsonFormat1(StanfordPostaggerInitializer.apply)
  private implicit val simpleInitFormat = jsonFormat1(SimplePostaggerInitializer.apply)
  private implicit val experimentInitFormat = jsonFormat2(SwappablePostaggerInitializer.apply)

  implicit val postaggerInitJsonFormat = parentFormat[PolyPostaggerInitializer](
    childFormat[FactoriePostaggerInitializer, PolyPostaggerInitializer],
    childFormat[StanfordPostaggerInitializer, PolyPostaggerInitializer],
    childFormat[SimplePostaggerInitializer, PolyPostaggerInitializer],
    childFormat[SwappablePostaggerInitializer, PolyPostaggerInitializer]
  )
}
