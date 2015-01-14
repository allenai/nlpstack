package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm._
import spray.json._

class ParserClassificationTaskSpec extends UnitSpec {
  // scalastyle:off

  /** This represents the following parser state:
    * format: OFF
    *
    *   ---------
    *   |        |
    *   |        V
    * NEXUS_0  saw_2  a_3  white_4  cat_5 |||  with_6  a_7  telescope_8
    *           |
    *           V
    *          we_1
    */
  val state1: TransitionParserState = TransitionParserState(
    stack = Vector(5, 4, 3, 2, 0),
    bufferPosition = 6,
    breadcrumb = Map(0 -> -1, 1 -> 2, 2 -> 0),
    children = Map(0 -> Set(2), 2 -> Set(1)),
    arcLabels = Map(Set(0, 2) -> 'root, Set(2, 1) -> 'nsubj),
    annotatedSentence = AnnotatedSentence(
      Sentence(Vector(NexusToken, Token('we), Token('saw), Token('a),
        Token('white), Token('cat), Token('with, Map('cpos -> Set('prep))), Token('a),
        Token('telescope))),
      IndexedSeq()
    )
  )

  "Serializing a ApplicabilitySignature" should "preserve it" in {
    val applicabilitySig: ClassificationTask = ApplicabilitySignature(true, false, false, true)
    applicabilitySig.toJson.convertTo[ClassificationTask] shouldBe applicabilitySig
  }

  "Serializing a StateRefCpos" should "preserve it" in {
    val bufferCpos: ClassificationTask = StateRefProperty(BufferRef(0), 'cpos, "det")
    bufferCpos.toJson.convertTo[ClassificationTask] shouldBe bufferCpos
  }

  "Calling ApplicabilitySignatureIdentifier's apply" should "return state1's applicability" in {
    ApplicabilitySignatureIdentifier(state1) shouldBe
      Some(ApplicabilitySignature(true, false, true, true))
  }

  "Serializing an ApplicabilitySignatureIdentifier" should "preserve it" in {
    val taskIdentifier: TaskIdentifier = ApplicabilitySignatureIdentifier
    taskIdentifier.toJson.convertTo[TaskIdentifier] shouldBe taskIdentifier
  }

  "Calling BufferCposIdentifier's apply" should "return state1's buffer top" in {
    StateRefPropertyIdentifier(BufferRef(0), 'cpos)(state1) shouldBe
      Some(StateRefProperty(BufferRef(0), 'cpos, "prep"))
  }

  "Serializing a BufferCposIdentifier" should "preserve it" in {
    val taskIdentifier: TaskIdentifier = StateRefPropertyIdentifier(BufferRef(0), 'cpos)
    taskIdentifier.toJson.convertTo[TaskIdentifier] shouldBe taskIdentifier
  }

  "Calling TaskConjunctionIdentifier's apply" should "return the proper conjunction" in {
    val taskIdentifier: TaskIdentifier = TaskConjunctionIdentifier(List(
      ApplicabilitySignatureIdentifier, StateRefPropertyIdentifier(BufferRef(0), 'cpos)
    ), None)
    taskIdentifier(state1) shouldBe
      Some(TaskConjunction(List(
        ApplicabilitySignature(true, false, true, true),
        StateRefProperty(BufferRef(0), 'cpos, "prep")
      )))
  }

  "Serializing a TaskConjunctionIdentifier" should "preserve it" in {
    val taskIdentifier: TaskIdentifier = TaskConjunctionIdentifier(List(
      ApplicabilitySignatureIdentifier, StateRefPropertyIdentifier(BufferRef(0), 'cpos)
    ), None)
    taskIdentifier.toJson.convertTo[TaskIdentifier] shouldBe taskIdentifier
  }

}
