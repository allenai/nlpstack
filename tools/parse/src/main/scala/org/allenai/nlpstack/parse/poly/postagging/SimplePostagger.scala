package org.allenai.nlpstack.parse.poly.postagging

import java.io._

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm._
import reming.CompactPrinter
import reming.DefaultJsonProtocol._

/** A part-of-speech tagger based on the org.allenai.nlpstack.parse.poly.fsm package.
  *
  * @param costFunctionFactory for each sentence, builds a cost function over FSM transitions
  * @param nbestSize number of taggings to generate (per sentence)
  */
case class SimplePostagger(
    costFunctionFactory: StateCostFunctionFactory,
    nbestSize: Int
) extends SentenceTagger {

  override def tag(
    sentence: Sentence
  ): TaggedSentence = {

    val constraints = Set[TransitionConstraint]() // no constraints are permitted here
    val costFunction =
      costFunctionFactory.buildCostFunction(sentence, constraints)
    val baseParser = new NbestSearch(costFunction)
    val nbestList: Option[NbestList] =
      costFunction.transitionSystem.initialState(
        constraints.toSeq
      ) map { initState =>
        baseParser.find(initState, nbestSize, constraints)
      }
    val bestCandidates: Option[Seq[TaggedSentence]] = nbestList map { nbList =>
      (nbList.scoredSculptures flatMap {
        case (taggedSent: TaggedSentence, _) =>
          Some(taggedSent)
        case _ =>
          None
      }).toSeq
    }
    // merge all "good" tags into a single tagged sentence, then return
    TaggedSentence(
      sentence,
      if (bestCandidates == None) {
        Map[Int, Set[TokenTag]]()
      } else {
        (bestCandidates.get flatMap { candidate =>
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
      }
    )
  }
}

object SimplePostagger {

  implicit val postaggerJsonFormat = jsonFormat2(SimplePostagger.apply)

  /** Save a SimplePostagger to disk.
    *
    * @param tagger the postagger you want to save
    * @param filePrefix the file prefix (".tagger.json" will be automatically appended)
    */
  def save(tagger: SimplePostagger, filePrefix: String): Unit = {
    val filename = filePrefix + ".tagger.json"
    Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { writer =>
      CompactPrinter.printTo(writer, tagger)
    }
  }

  /** Load a SimplePostagger from disk.
    *
    * @param filename the file contains the serialized postagger
    * @param overrideNbestSize override the serialized 'nbestSize' parameter, if desired
    * @return the initialized postagger
    */
  def load(filename: String, overrideNbestSize: Option[Int] = None): SimplePostagger = {
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
      overrideNbestSize match {
        case Some(nbestSize) =>
          SimplePostagger(result.costFunctionFactory, nbestSize)
        case None =>
          result
      }
    }
    Resource.using(new File(filename).toURI.toURL.openStream()) { loadFromStream }
  }
}
