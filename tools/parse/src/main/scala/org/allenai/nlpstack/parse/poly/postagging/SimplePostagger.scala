package org.allenai.nlpstack.parse.poly.postagging

import java.io._

import org.allenai.common.Resource
import org.allenai.nlpstack.core.Postagger
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.eval.{ UnlabeledBreadcrumbAccuracy, TaggingEvaluator, CposSentAccuracy, EvaluationStatistic }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.polyparser._
import org.allenai.nlpstack.postag.{ StanfordPostagger, defaultPostagger }
import reming.CompactPrinter
import reming.DefaultJsonProtocol._

import scala.compat.Platform
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

trait PolyPostagger {
  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence]
}

sealed trait PolyPostaggerInitializer

case object FactoriePostaggerInitializer extends PolyPostaggerInitializer
case object StanfordPostaggerInitializer extends PolyPostaggerInitializer
case class SimplePostaggerInitializer(configFile: String) extends PolyPostaggerInitializer

object PolyPostaggerInitializer {
  private implicit val factorieInitFormat = jsonFormat0(() => FactoriePostaggerInitializer)
  private implicit val stanfordInitFormat = jsonFormat0(() => StanfordPostaggerInitializer)
  private implicit val simpleInitFormat = jsonFormat1(SimplePostaggerInitializer.apply)

  implicit val postaggerInitJsonFormat = parentFormat[PolyPostaggerInitializer](
    childFormat[FactoriePostaggerInitializer.type, PolyPostaggerInitializer],
    childFormat[StanfordPostaggerInitializer.type, PolyPostaggerInitializer],
    childFormat[SimplePostaggerInitializer, PolyPostaggerInitializer]
  )
}

object PolyPostagger {

  def initializePostagger(initializer: PolyPostaggerInitializer): PolyPostagger = {
    initializer match {
      case FactoriePostaggerInitializer =>
        NLPStackPostagger(defaultPostagger)
      case StanfordPostaggerInitializer =>
        NLPStackPostagger(new StanfordPostagger())
      case SimplePostaggerInitializer(configFile) =>
        SimplePostagger.load(configFile)
    }
  }
}

case class NLPStackPostagger(baseTagger: Postagger) extends PolyPostagger {
  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence] = {

    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, baseTagger)
    val tagMap = (taggedTokens.zipWithIndex map {
      case (tok, tokIndex) =>
        (tokIndex + 1, //Set(Symbol(tok.postag)))
          Set(Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(tok.postag, "X"))))
    }).toMap
    Some(TaggedSentence(sentence, tagMap))
  }
}

object SimplePostagger {
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
      CposSentAccuracy(false)
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
    println("Parsed %d sentences in %.1f seconds, an average of %.1f sentences per second.".format(
      UnlabeledBreadcrumbAccuracy.numParses, parsingDurationInSeconds,
      (1.0 * UnlabeledBreadcrumbAccuracy.numParses) / parsingDurationInSeconds
    ))
  }

  implicit val postaggerJsonFormat = jsonFormat3(SimplePostagger.apply)

  def save(tagger: SimplePostagger, filePrefix: String): Unit = {
    val filename = filePrefix + ".tagger.json"
    Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { writer =>
      CompactPrinter.printTo(writer, tagger)
    }
  }

  /** Load a parser from a file.
    *
    * @param filename the file contains the serialized parser
    * @return the initialized parser
    */
  def load(filename: String): SimplePostagger = {
    Resource.using(new File(filename).toURI.toURL.openStream()) { loadFromStream }
  }

  def loadFromStream(stream: InputStream): SimplePostagger = {
    println("Loading tagger.")
    System.gc()
    val initialMemory = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    val result = Util.readFromStream[SimplePostagger](stream)
    System.gc()
    val memoryAfterLoading = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    println("Parser memory footprint: %.1f MB".format(
      (memoryAfterLoading - initialMemory).toDouble / Math.pow(10.0, 6.0)
    ))
    result
  }
}

case class SimplePostagger(
    costFunctionFactory: StateCostFunctionFactory,
    rerankingFunction: RerankingFunction, nbestSize: Int
) extends PolyPostagger {

  @transient val reranker: Reranker = new Reranker(rerankingFunction)

  def tagWithScore(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[(TaggedSentence, Double)] = {

    val costFunction =
      costFunctionFactory.buildCostFunction(sentence, constraints)
    val baseParser = new NbestSearch(costFunction)
    val nbestList: Option[NbestList] =
      costFunction.transitionSystem.initialState(
        constraints.toSeq
      ) map { initState =>
        // Only do full reranking in the absence of constraints.
        val nbestsizeMod = 5
        if (constraints.isEmpty) {
          baseParser.find(initState, nbestsizeMod, constraints)
        } else {
          baseParser.find(initState, 1, constraints)
        }
      }
    val mappedNbestList: Option[NbestList] = nbestList map { x =>
      NbestList(x.scoredSculptures)
    }
    val bestCandidates: Option[Seq[TaggedSentence]] = mappedNbestList map { nbList =>
      (nbList.scoredSculptures flatMap {
        case (taggedSent: TaggedSentence, _) =>
          Some(taggedSent)
        case _ =>
          None
      }).toSeq
    }
    val mergedCandidates: Option[TaggedSentence] = bestCandidates map { bestCands =>
      TaggedSentence(
        bestCands.head.sentence,
        (bestCands flatMap { candidate =>
          candidate.tags.toSeq
        }) groupBy {
          case (tokenIndex, _) =>
            tokenIndex
        } mapValues {
          case value =>
            (value map {
              case (_, tagset) =>
                tagset
            }).flatten.toSet
        }
      )
    }
    mergedCandidates map { cand => (cand, 0.0) }
    /*
    val candidate: Option[(Sculpture, Double)] = mappedNbestList flatMap { nbList =>
      reranker.rerankWithScore(nbList)
    }
    candidate match {
      case Some((taggedSentence: TaggedSentence, cost)) =>
        Some((taggedSentence, cost))
      case _ => None
    }
    */
  }

  def tag(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[TaggedSentence] = {

    tagWithScore(sentence, constraints) map { case (tagged, _) => tagged }
  }
}

