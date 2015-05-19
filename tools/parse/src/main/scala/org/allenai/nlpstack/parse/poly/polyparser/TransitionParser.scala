package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence, Util }
import org.allenai.nlpstack.parse.poly.fsm.{ ClassifierBasedCostFunctionFactory, TransitionConstraint }
import reming.{ CompactPrinter, JsonParser, LazyFormat }
import reming.DefaultJsonProtocol._

import java.io.{ BufferedWriter, InputStream, File, FileWriter, PrintWriter }

import scala.io.BufferedSource

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

  /** Returns all possible arc labels for this transition parser.
    *
    * TODO: currently this is hard-coded for Stanford labels. This should be revised to get these
    * labels from the trained models.
    *
    * @return the set of all possible arc labels that the parser might use
    */
  def possibleArcLabels: Set[Symbol] = {
    Set('ACOMP, 'ADVCL, 'ADVMOD, 'AMOD, 'APPOS,
      'AUX, 'AUXPASS, 'CC, 'CCOMP, 'CONJ, 'COP, 'CSUBJ, 'CSUBJPASS, 'DEP,
      'DET, 'DISCOURSE, 'DOBJ, 'EXPL, 'GOESWITH, 'IOBJ, 'MARK, 'MWE, 'NEG,
      'NN, 'NPADVMOD, 'NSUBJ, 'NSUBJPASS, 'NUM, 'NUMBER, 'PARATAXIS, 'PCOMP, 'POBJ, 'POSS,
      'POSSESSIVE, 'PRECONJ, 'PREDET,
      'PREP, 'PRT, 'PUNCT, 'QUANTMOD, 'RCMOD, 'ROOT, 'TMOD, 'VMOD, 'XCOMP)
  }
}

object TransitionParser {
  implicit object TransitionParserFormat extends LazyFormat[TransitionParser] {
    private implicit val parseCacheFormat = jsonFormat2(ParseCache.apply)
    private implicit val rerankingParserFormat = jsonFormat1(RerankingTransitionParser.apply)

    override val delegate = parentFormat[TransitionParser](
      childFormat[ParseCache, TransitionParser],
      childFormat[RerankingTransitionParser, TransitionParser]
    )
  }

  /** Save a parser to a file.
    *
    * This will automatically append ".poly.json" to the specified prefix.
    *
    * @param parser the parser to save
    * @param modelFilePrefix the file to save the configuration to
    */
  def save(parser: TransitionParser, modelFilePrefix: String): Unit = {
    val filename = modelFilePrefix + ".poly.json"
    Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { writer =>
      CompactPrinter.printTo(writer, parser)
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
    val initialMemory = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    val result = Util.readFromStream[TransitionParser](stream)
    System.gc()
    val memoryAfterLoading = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    println("Parser memory footprint: %.1f MB".format(
      (memoryAfterLoading - initialMemory).toDouble / Math.pow(10.0, 6.0)
    ))
    result
  }
}

