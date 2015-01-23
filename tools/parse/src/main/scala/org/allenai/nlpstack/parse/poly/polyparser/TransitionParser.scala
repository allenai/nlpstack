package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ InputStream, File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence, Util }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A TransitionParser implements a parsing algorithm for a transition-based parser. */
abstract class TransitionParser {

  /** Given an initial state, this returns the "best" sequence of Transitions that one should
    * apply to the initial state in order to get a polytree parse. The adjective "best" depends
    * on the specific implementation of the TransitionParser.
    *
    * @param sentence the sentence you want to parse
    * @param constraints a set of ParserConstraint objects, which constrain the choices the parser
    * may make
    * @return the "best" list of Transitions you should apply to the initial state (or None if
    * a parse failure occurs)
    */
  def parse(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Option[PolytreeParse]

  def parseStringSequence(
    tokens: Seq[String],
    constraints: Set[TransitionConstraint] = Set()
  ): Option[PolytreeParse] = {

    // note that we invert this back into a rooted tree for this function, since that's what's
    // expected by nlpstack
    parse(
      Sentence((NexusToken +: (tokens map { tok => Token(Symbol(tok)) })).toIndexedSeq),
      constraints
    ) map { x => PolytreeParse.arcInverterStanford(x) }
  }
}

object TransitionParser {
  /** Boilerplate code to serialize a NeighborhoodExtractor to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM NeighborhoodExtractor, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object ParserJsonFormat extends RootJsonFormat[TransitionParser] {

    implicit val rerankingParserFormat =
      jsonFormat1(RerankingTransitionParser.apply).pack("type" -> "RerankingParser")

    implicit val parseCacheFormat =
      jsonFormat2(ParseCache.apply).pack("type" -> "ParseCache")

    def write(parser: TransitionParser): JsValue = parser match {
      case rerankingParser: RerankingTransitionParser =>
        rerankingParser.toJson
      case parseCache: ParseCache =>
        parseCache.toJson
    }

    def read(value: JsValue): TransitionParser = value match {
      case jsObj: JsObject => jsObj.unpackWith(rerankingParserFormat, parseCacheFormat)
      case _ => deserializationError("Unexpected JsValue type. Must be JsObject.")
    }
  }

  /** Save a parser to a file.
    *
    * This will automatically append ".poly.json" to the specified prefix.
    *
    * @param parser the parser to save
    * @param modelFilePrefix the file to save the configuration to
    */
  def save(parser: TransitionParser, modelFilePrefix: String): Unit = {
    Resource.using(new PrintWriter(new File(modelFilePrefix + ".poly.json"))) { writer =>
      writer.println(parser.toJson.prettyPrint)
    }
  }

  /** Load a parser from a file.
    *
    * @param filename the file contains the serialized parser
    * @return the initialized parser
    */
  def load(filename: String): TransitionParser = {
    Resource.using(new File(filename).toURI.toURL.openStream()) { loadFromStream }
  }

  def loadFromStream(stream: InputStream): TransitionParser = {
    println("Loading parser.")
    System.gc()
    val initialMemory: Double = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    val result = convertJsValueToConfig(Util.getJsValueFromStream(stream))
    System.gc()
    val memoryAfterLoading: Double = Runtime.getRuntime.totalMemory -
      Runtime.getRuntime.freeMemory()
    println("Parser memory footprint: %.2f GB".format((memoryAfterLoading - initialMemory)
      / Math.pow(2.0, 30.0)))
    result
  }

  private def convertJsValueToConfig(jsVal: JsValue): TransitionParser = {
    jsVal match {
      case JsObject(values) =>
      case _ => deserializationError("Unexpected JsValue type. Must be " +
        "JsObject.")
    }
    jsVal.convertTo[TransitionParser]
  }
}

