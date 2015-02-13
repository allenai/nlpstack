package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.StateTransition
import spray.json._
import spray.json.DefaultJsonProtocol._

class TransitionSpec extends UnitSpec {
  // scalastyle:off

  val leftArc: StateTransition = new ArcEagerLeftArc('dummy)

  val rightArc: StateTransition = new ArcEagerRightArc('dummy)

  val tokens1: Vector[Token] = Vector(NexusToken, Token('we), Token('saw), Token('a),
    Token('white), Token('cat), Token('with), Token('a), Token('telescope))

  val breadcrumb1: Map[Int, Int] = Map(0 -> -1, 1 -> 2, 2 -> 0)

  val children1: Map[Int, Set[Int]] = Map(0 -> Set(2), 2 -> Set(1))

  val arcLabels1: Map[Set[Int], Symbol] = Map(Set(0, 2) -> 'root, Set(2, 1) -> 'nsubj)

  val state1: TransitionParserState = TransitionParserState(Vector(2, 0), 3, breadcrumb1,
    children1, arcLabels1, AnnotatedSentence(Sentence(tokens1), IndexedSeq()))

  val state1b: TransitionParserState = TransitionParserState(Vector(4, 3, 2, 0), 5, breadcrumb1,
    children1, arcLabels1, AnnotatedSentence(Sentence(tokens1), IndexedSeq()))

  val tokens2: Vector[Token] = Vector(NexusToken, Token('we), Token('saw), Token('it))

  val breadcrumb2: Map[Int, Int] = Map(0 -> -1, 1 -> 2, 2 -> 0, 3 -> 2)

  val children2: Map[Int, Set[Int]] = Map(0 -> Set(2), 2 -> Set(1, 3))

  val arcLabels2: Map[Set[Int], Symbol] = Map(Set(0, 2) -> 'root, Set(2, 1) ->
    'nsubj, Set(2, 3) -> 'dobj)

  val state2a: TransitionParserState = TransitionParserState(Vector(2, 0), 3, breadcrumb2,
    children2, arcLabels2, AnnotatedSentence(Sentence(tokens2), IndexedSeq()))

  val state2b: TransitionParserState = TransitionParserState(Vector(3, 2, 0), 4, breadcrumb2,
    children2, arcLabels2, AnnotatedSentence(Sentence(tokens2), IndexedSeq()))

  val state2c: TransitionParserState = TransitionParserState(Vector(), 3, breadcrumb2,
    children2, arcLabels2, AnnotatedSentence(Sentence(tokens2), IndexedSeq()))

  val state2d: TransitionParserState = TransitionParserState(Vector(0), 3, breadcrumb2,
    children2, arcLabels2, AnnotatedSentence(Sentence(tokens2), IndexedSeq()))

  val tokens3: Vector[Token] = Vector(NexusToken, Token('we), Token('saw), Token('it), Token('fast))

  val breadcrumb3: Map[Int, Int] = Map(0 -> -1, 1 -> 2, 3 -> 2)

  val children3: Map[Int, Set[Int]] = Map(2 -> Set(1, 3))

  val arcLabels3: Map[Set[Int], Symbol] = Map(Set(2, 1) -> 'nsubj, Set(2, 3) -> 'dobj)

  val state3a: TransitionParserState = TransitionParserState(Vector(3, 2, 0), 4, breadcrumb3,
    children3, arcLabels3, AnnotatedSentence(Sentence(tokens3), IndexedSeq()))

  "Calling shift's apply" should "give back a shifted state" in {
    ArcEagerShift(Some(state1)) shouldBe
      Some(TransitionParserState(Vector(3, 2, 0), 4, state1.breadcrumb,
        state1.children, state1.arcLabels, state1.annotatedSentence))
  }

  "Calling reduce's apply" should "give back a reduced state" in {
    ArcEagerReduce(Some(state2b)) shouldBe
      Some(TransitionParserState(Vector(2, 0), 4, state2b.breadcrumb,
        state2b.children, state2b.arcLabels, state2b.annotatedSentence))
  }

  "Calling left arc's apply" should "have a left arc in the returned state" in {
    leftArc(Some(state1b)) shouldBe
      Some(TransitionParserState(Vector(3, 2, 0), 5,
        state1b.breadcrumb + (4 -> 5),
        state1b.children,
        state1b.arcLabels + (Set(4, 5) -> 'dummy),
        state1b.annotatedSentence,
        Some(5 -> 4),
        DependencyParserModes.LEFTLABEL))
  }

  "Calling right arc's apply" should "have a right arc in the returned state" in {
    rightArc(Some(state1b)) shouldBe
      Some(TransitionParserState(Vector(5, 4, 3, 2, 0), 6,
        state1b.breadcrumb + (5 -> 4),
        state1b.children,
        state1b.arcLabels + (Set(4, 5) -> 'dummy),
        state1b.annotatedSentence,
        Some(4 -> 5),
        DependencyParserModes.RIGHTLABEL))
  }

  "Calling shift's applicable" should "return false for 2A since you can't shift the final buffer " +
    "item" in {
      StateTransition.applicable(ArcEagerShift, Some(state2a)) shouldBe false
    }

  it should "return false for 2B since the buffer is empty" in {
    StateTransition.applicable(ArcEagerShift, Some(state2b)) shouldBe false
  }

  it should "return false for 2C since you can't shift the final buffer item" in {
    StateTransition.applicable(ArcEagerShift, Some(state2c)) shouldBe false
  }

  it should "return false for 2D since you can't shift the final buffer item" in {
    StateTransition.applicable(ArcEagerShift, Some(state2d)) shouldBe false
  }

  it should "return true for 1B since its preconds are satisfied" in {
    StateTransition.applicable(ArcEagerShift, Some(state1b)) shouldBe true
  }

  it should "return false for 3A since there exists a stack item without a breadcrumb" in {
    StateTransition.applicable(ArcEagerShift, Some(state3a)) shouldBe false
  }

  "Calling reduce's applicable" should "return false for 2A since its stack top's breadcrumb" +
    "is the nexus and there are still items left in the buffer" in {
      StateTransition.applicable(ArcEagerReduce, Some(state2a)) shouldBe false
    }

  it should "return true for 2B since its preconds are satisfied" in {
    StateTransition.applicable(ArcEagerReduce, Some(state2b)) shouldBe true
  }

  it should "return false for 2C since the stack is empty" in {
    StateTransition.applicable(ArcEagerReduce, Some(state2c)) shouldBe false
  }

  it should "return false for 2D since the stack top is the zeroth token" in {
    StateTransition.applicable(ArcEagerReduce, Some(state2d)) shouldBe false
  }

  it should "return false for 1B since the stack top has no breadcrumb" in {
    StateTransition.applicable(ArcEagerReduce, Some(state1b)) shouldBe false
  }

  it should "return true for 3A since its preconds are satisfied" in {
    StateTransition.applicable(ArcEagerReduce, Some(state3a)) shouldBe true
  }

  "Calling left arc's applicable" should "return false for 2A since the stack head has a " +
    "breadcrumb" in {
      StateTransition.applicable(leftArc, Some(state2a)) shouldBe false
    }

  it should "return false for 2B since the buffer is empty" in {
    StateTransition.applicable(leftArc, Some(state2b)) shouldBe false
  }

  it should "return false for 2C since the stack is empty" in {
    StateTransition.applicable(leftArc, Some(state2c)) shouldBe false
  }

  it should "return false for 2D since the stack head has a breadcrumb already" in {
    StateTransition.applicable(leftArc, Some(state2d)) shouldBe false
  }

  it should "return true for 1B since its preconds are satisfied" in {
    StateTransition.applicable(leftArc, Some(state1b)) shouldBe true
  }

  it should "return false for 3A since the stack head has a breadcrumb already" in {
    StateTransition.applicable(leftArc, Some(state3a)) shouldBe false
  }

  "Calling right arc's applicable" should "return true for 2A since its preconds are " +
    "satisfied" in {
      StateTransition.applicable(rightArc, Some(state2a)) shouldBe true
    }

  it should "return false for 2B since the buffer is empty" in {
    StateTransition.applicable(rightArc, Some(state2b)) shouldBe false
  }

  it should "return false for 2C since the stack is empty" in {
    StateTransition.applicable(rightArc, Some(state2c)) shouldBe false
  }

  it should "return true for 2D since its preconds are satisfied" in {
    StateTransition.applicable(rightArc, Some(state2d)) shouldBe true
  }

  it should "return true for 1B since its preconds are satisfied" in {
    StateTransition.applicable(rightArc, Some(state1b)) shouldBe true
  }

  it should "return false for 3A since there exists a stack item without a breadcrumb" in {
    StateTransition.applicable(rightArc, Some(state3a)) shouldBe false
  }

  "Serializing a list of Transitions" should "preserve it" in {
    val transitions: List[StateTransition] = List(ArcEagerShift, ArcEagerReduce,
      ArcEagerLeftArc('a), ArcEagerRightArc('b))
    transitions.toJson.convertTo[List[StateTransition]] shouldBe transitions
  }
}
