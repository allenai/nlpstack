package org.allenai.nlpstack.parse.poly.fsm
// scalastyle:off

import org.allenai.common.testkit.UnitSpec

case class StringState(str: String) extends State {
  override val isFinal = false
  override def asSculpture: Option[Sculpture] = ???
}

case class FirstLetterTask(firstLetter: String) extends ClassificationTask {
  override val filenameFriendlyName: String = "firstLetterTask"
}

case object FirstLetterTaskIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case StringState(str) if str.size > 0 =>
        Some(FirstLetterTask(str.substring(0, 1)))
      case StringState(str) =>
        Some(FirstLetterTask("-"))
      case _ => None
    }
  }
}

case class SecondLetterTask(secondLetter: String) extends ClassificationTask {
  override val filenameFriendlyName: String = "secondLetterTask"
}

case object SecondLetterTaskIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case StringState(str) if str.size > 1 =>
        Some(SecondLetterTask(str.substring(1, 2)))
      case StringState(str) =>
        Some(SecondLetterTask("-"))
      case _ => None
    }
  }
}

class ClassificationTaskSpec extends UnitSpec {

  val taskConjunctionIdentifier1 = new TaskConjunctionIdentifier(
    taskIdentifiers = List(FirstLetterTaskIdentifier, SecondLetterTaskIdentifier),
    activeTaskConjuncts =
      Some(Set(
        TaskConjunction(Seq()),
        TaskConjunction(Seq(FirstLetterTask("c"))),
        TaskConjunction(Seq(FirstLetterTask("m"), SecondLetterTask("e"))),
        TaskConjunction(Seq(FirstLetterTask("c"), SecondLetterTask("a")))
      ))
  )

  "TaskConjunctionIdentifier's .apply method" should "find the right results" in {
    taskConjunctionIdentifier1(StringState("mental")) shouldBe
      Some(TaskConjunction(Seq(FirstLetterTask("m"), SecondLetterTask("e"))))
    taskConjunctionIdentifier1(StringState("mintal")) shouldBe
      Some(TaskConjunction(Seq()))
    taskConjunctionIdentifier1(StringState("cannot")) shouldBe
      Some(TaskConjunction(Seq(FirstLetterTask("c"), SecondLetterTask("a"))))
    taskConjunctionIdentifier1(StringState("central")) shouldBe
      Some(TaskConjunction(Seq(FirstLetterTask("c"))))
    taskConjunctionIdentifier1(StringState("dental")) shouldBe
      Some(TaskConjunction(Seq()))
  }

  "TaskConjunctionIdentifier.learn" should "induce a correct TaskConjunctionIdentifier" in {
    val identifiers = List(FirstLetterTaskIdentifier, SecondLetterTaskIdentifier)
    val states = Seq("mental", "rental", "maternal", "meme", "rash", "mojave",
      "sequence", "alphabet", "rested", "rational", "rotation") map { str => StringState(str) }
    val stateSource = InMemoryStateSource(states)
    val taskIdentifier = TaskConjunctionIdentifier.learn(identifiers, stateSource, 2)
    taskIdentifier(StringState("mended")).get shouldBe
      TaskConjunction(List(FirstLetterTask("m"), SecondLetterTask("e")))
    taskIdentifier(StringState("minted")).get shouldBe
      TaskConjunction(List(FirstLetterTask("m")))
    taskIdentifier(StringState("random")).get shouldBe
      TaskConjunction(List(FirstLetterTask("r"), SecondLetterTask("a")))
    taskIdentifier(StringState("ash")).get shouldBe
      TaskConjunction(List())
    taskIdentifier(StringState("rooibos")).get shouldBe
      TaskConjunction(List())
  }
}
