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
        TaskConjunction(Seq(FirstLetterTask("c"), SecondLetterTask("a")))))
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


  val taskTree1 = new TaskTree(Some(FirstLetterTaskIdentifier),
    List(
      (FirstLetterTask("c"), new TaskTree(None, List())),
      (FirstLetterTask("r"), new TaskTree(None, List())),
      (FirstLetterTask("m"),
        new TaskTree(Some(SecondLetterTaskIdentifier),
          List(
            (SecondLetterTask("e"), new TaskTree(None, List())),
            (SecondLetterTask("o"), new TaskTree(None, List())))))))

  "TaskTree's .identifyTaskConjunction" should "find the right results for taskTree1" in {
    taskTree1.identifyTaskConjunction(StringState("mental")) shouldBe
      TaskConjunction(List(FirstLetterTask("m"), SecondLetterTask("e")))
    taskTree1.identifyTaskConjunction(StringState("mintal")) shouldBe
      TaskConjunction(List(FirstLetterTask("m")))
    taskTree1.identifyTaskConjunction(StringState("rental")) shouldBe
      TaskConjunction(List(FirstLetterTask("r")))
    taskTree1.identifyTaskConjunction(StringState("dental")) shouldBe
      TaskConjunction(List())
  }

  "TaskTree.learnTaskTree" should "induce a correct TaskTree" in {
    val identifiers = IndexedSeq(FirstLetterTaskIdentifier, SecondLetterTaskIdentifier)
    val states = Seq("mental", "rental", "meme", "rash", "sequence", "alphabet", "rested",
      "rational", "rotation") map { str => StringState(str) }
    val stateSource = InMemoryStateSource(states)
    val taskTree = TaskTree.learnTaskTree(identifiers, stateSource.getStateIterator, 2)
    taskTree.identifyTaskConjunction(StringState("mended")) shouldBe
      TaskConjunction(List(FirstLetterTask("m"), SecondLetterTask("e")))
    taskTree.identifyTaskConjunction(StringState("minted")) shouldBe
      TaskConjunction(List(FirstLetterTask("m")))
    taskTree.identifyTaskConjunction(StringState("random")) shouldBe
      TaskConjunction(List(FirstLetterTask("r"), SecondLetterTask("a")))
    taskTree.identifyTaskConjunction(StringState("ash")) shouldBe
      TaskConjunction(List())
  }

  "TaskTree.learnTaskTree" should "not crash when nothing qualifies" in {
    val identifiers = IndexedSeq(FirstLetterTaskIdentifier, SecondLetterTaskIdentifier)
    val states = Seq("mental", "mesh", "meme") map { str => StringState(str) }
    val stateSource = InMemoryStateSource(states)
    val taskTree = TaskTree.learnTaskTree(identifiers, stateSource.getStateIterator, 4)
    taskTree.identifyTaskConjunction(StringState("mended")) shouldBe
      TaskConjunction(List())
    taskTree.identifyTaskConjunction(StringState("ash")) shouldBe
      TaskConjunction(List())
  }
}
