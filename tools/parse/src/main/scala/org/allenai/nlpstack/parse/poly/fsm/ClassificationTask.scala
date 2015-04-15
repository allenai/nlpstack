package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser._

import reming.LazyFormat
import reming.DefaultJsonProtocol._

/** A ClassificationTask specifies a particular classification task for which we want
  * to collect feature vectors and train a classifier.
  *
  * In practice, feature vectors will be tagged with their ClassificationTask, allowing us
  * to easily sort a mixed set of feature vectors according to their relevant
  * ClassificationTask, before training the respective classifiers.
  */
abstract class ClassificationTask {
  val filenameFriendlyName: String
}

object ClassificationTask {
  implicit object ClassificationTaskJsonFormat extends LazyFormat[ClassificationTask] {
    private implicit val applicabilitySignatureFormat = jsonFormat4(ApplicabilitySignature.apply)
    private implicit val stateRefPropertyFormat = jsonFormat3(StateRefProperty.apply)
    private implicit val simpleTaskFormat = jsonFormat1(SimpleTask.apply)
    private implicit val taskConjunctionFormat = jsonFormat1(TaskConjunction.apply)

    override val delegate = parentFormat[ClassificationTask](
      childFormat[ApplicabilitySignature, ClassificationTask],
      childFormat[StateRefProperty, ClassificationTask],
      childFormat[TaskConjunction, ClassificationTask],
      childFormat[SimpleTask, ClassificationTask]
    )
  }
}

case class SimpleTask(val taskName: String) extends ClassificationTask {
  @transient
  override val filenameFriendlyName: String = s"$taskName"
}

/** The TaskConjunction is a conjunction of ClassificationTasks.
  *
  * @param tasks the tasks we want to conjoin
  */
case class TaskConjunction(val tasks: Seq[ClassificationTask]) extends ClassificationTask {

  /** Returns an identifier that can be used in a filename. */
  @transient
  override val filenameFriendlyName: String = {
    if (tasks.nonEmpty) {
      (tasks map { task => task.filenameFriendlyName }).mkString(".")
    } else {
      "rootTask"
    }
  }
}

/** A TaskIdentifier identifies the ClassificationTask required to determine the next transition
  * from a given parser state.
  */
trait TaskIdentifier extends (State => Option[ClassificationTask])

object TaskIdentifier {
  implicit object TaskIdentifierJsonFormat extends LazyFormat[TaskIdentifier] {
    private implicit val arcEagerTaskIdentifierFormat = jsonFormat0(() => ArcEagerTaskIdentifier)
    private implicit val arcHybridTaskIdentifierFormat = jsonFormat0(() => ArcHybridTaskIdentifier)
    private implicit val stateRefPropertyIdentifierFormat =
      jsonFormat2(StateRefPropertyIdentifier.apply)
    private implicit val taskConjunctionIdentifierFormat =
      jsonFormat2(TaskConjunctionIdentifier.apply)

    override val delegate = parentFormat[TaskIdentifier](
      childFormat[ArcEagerTaskIdentifier.type, TaskIdentifier],
      childFormat[ArcHybridTaskIdentifier.type, TaskIdentifier],
      childFormat[StateRefPropertyIdentifier, TaskIdentifier],
      childFormat[TaskConjunctionIdentifier, TaskIdentifier]
    )
  }
}

/** The SimpleTaskIdentifier identifies all states as the same SimpleTask.
  *
  * @param taskName the name of the task
  */
case class SimpleTaskIdentifier(taskName: String) extends TaskIdentifier {

  override def apply(state: State): Option[ClassificationTask] = Some(SimpleTask(taskName))
}

/** The TaskConjunctionIdentifier allows you to create a TaskIdentifier by conjoining
  * existing TaskIdentifiers. You will want to do this if you want to partition feature vectors
  * according to multiple criteria at once.
  *
  * @param taskIdentifiers the task identifiers you want to conjoin
  */
case class TaskConjunctionIdentifier(
    taskIdentifiers: List[TaskIdentifier],
    activeTaskConjuncts: Option[Set[ClassificationTask]]
) extends TaskIdentifier {

  override def apply(state: State): Option[ClassificationTask] = {
    val optTasks: Seq[Option[ClassificationTask]] = taskIdentifiers map { _(state) }
    if (optTasks.contains(None)) {
      None
    } else {
      val tasks: Seq[ClassificationTask] = optTasks.flatten
      val possibleTaskConjuncts = (0 to tasks.size) map { i => TaskConjunction(tasks.take(i)) }
      activeTaskConjuncts match {
        case Some(activeTConjuncts) =>
          (possibleTaskConjuncts filter { conjunct =>
            activeTConjuncts.contains(conjunct)
          }).lastOption
        case None =>
          possibleTaskConjuncts.lastOption
      }
    }
  }
}

object TaskConjunctionIdentifier {

  def learn(taskIdentifiers: List[TaskIdentifier], goldStates: StateSource,
    activationThreshold: Int): TaskConjunctionIdentifier = {

    val taskConjunctionIdentifiers: Seq[TaskConjunctionIdentifier] =
      (0 to taskIdentifiers.size) map { i =>
        TaskConjunctionIdentifier(taskIdentifiers.take(i), None)
      }

    // get task tree node histogram: each gold parser state corresponds to a node of the full task
    // tree; in the following lines, we want to count how many parser states correspond to each
    // task tree node, then we filter out the tasks that do not have a qualifying number of
    // corresponding gold states
    val classificationTasks: Iterator[ClassificationTask] = for {
      parserState <- goldStates.getStateIterator
      taskConjunctionIdentifier <- taskConjunctionIdentifiers
    } yield (taskConjunctionIdentifier(parserState).get)
    var taskCounts: Map[ClassificationTask, Int] = Map[ClassificationTask, Int]()
    classificationTasks foreach { task =>
      taskCounts = taskCounts.updated(task, 1 + taskCounts.getOrElse(task, 0))
    }
    val filteredTasks: Set[ClassificationTask] = (taskCounts filter
      { case (task, count) => (count >= activationThreshold) }).keySet

    val activatedTasks: Iterator[ClassificationTask] = goldStates.getStateIterator map { state =>
      val stateTasks: Seq[ClassificationTask] = taskConjunctionIdentifiers flatMap { identifier =>
        identifier(state)
      }
      val filteredStateTasks = stateTasks filter { task => filteredTasks.contains(task) }
      filteredStateTasks.last
    }
    var activatedTaskCounts: Map[ClassificationTask, Int] = Map[ClassificationTask, Int]()
    activatedTasks foreach { task =>
      activatedTaskCounts =
        activatedTaskCounts.updated(task, 1 + activatedTaskCounts.getOrElse(task, 0))
    }
    val finalTasks: Set[ClassificationTask] = ((activatedTaskCounts filter
      {
        case (task, count) =>
          (count >= activationThreshold)
      }).keySet) union Set(TaskConjunction(Seq()))

    TaskConjunctionIdentifier(taskIdentifiers, Some(finalTasks))
  }
}
