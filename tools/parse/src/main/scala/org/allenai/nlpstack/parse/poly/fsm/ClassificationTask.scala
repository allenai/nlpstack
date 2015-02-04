package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser._
import org.allenai.common.json._

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.annotation.tailrec
import scala.collection.mutable

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

  /** Boilerplate code to serialize a ClassificationTask to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM ClassificationTask, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object ClassificationTaskJsonFormat extends RootJsonFormat[ClassificationTask] {
    implicit val applicabilitySignatureFormat =
      jsonFormat4(ApplicabilitySignature.apply).pack("type" -> "ApplicabilitySignature")

    implicit val stateRefPropertyFormat =
      jsonFormat3(StateRefProperty.apply).pack("type" -> "StateRefProperty")

    implicit val taskConjunctionFormat =
      jsonFormat1(TaskConjunction.apply).pack("type" -> "TaskConjunction")

    implicit val simpleTaskFormat =
      jsonFormat1(SimpleTask.apply).pack("type" -> "SimpleTask")

    def write(task: ClassificationTask): JsValue = task match {
      case appSignature: ApplicabilitySignature => appSignature.toJson
      case stateRefProperty: StateRefProperty => stateRefProperty.toJson
      case taskConjunction: TaskConjunction => taskConjunction.toJson
      case simpleTask: SimpleTask => simpleTask.toJson
    }

    def read(value: JsValue): ClassificationTask = value.asJsObject.unpackWith(
      applicabilitySignatureFormat,
      stateRefPropertyFormat,
      taskConjunctionFormat,
      simpleTaskFormat
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

object TaskConjunction {
  implicit val jsFormat = jsonFormat1(TaskConjunction.apply)
}

/** A TaskIdentifier identifies the ClassificationTask required to determine the next transition
  * from a given parser state.
  */
trait TaskIdentifier extends (State => Option[ClassificationTask])

object TaskIdentifier {

  /** Boilerplate code to serialize a TaskIdentifier to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM TaskIdentifier, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object TaskIdentifierJsonFormat extends RootJsonFormat[TaskIdentifier] {

    implicit val stateRefPropertyIdentifierFormat =
      jsonFormat2(StateRefPropertyIdentifier.apply).pack("type" -> "StateRefPropertyIdentifier")

    implicit val taskConjunctionIdentifierFormat =
      jsonFormat2(TaskConjunctionIdentifier.apply).pack("type" -> "TaskConjunctionIdentifier")

    def write(taskIdentifier: TaskIdentifier): JsValue = taskIdentifier match {
      case ApplicabilitySignatureIdentifier => JsString("ApplicabilitySignatureIdentifier")
      case ArcHybridTaskIdentifier =>
        JsString("HybridTaskIdentifier")
      case stateRefPropertyIdentifier: StateRefPropertyIdentifier =>
        stateRefPropertyIdentifier.toJson
      case taskIdentifier: TaskConjunctionIdentifier => taskIdentifier.toJson
    }

    def read(value: JsValue): TaskIdentifier = value match {
      case JsString(typeid) => typeid match {
        case "ApplicabilitySignatureIdentifier" => ApplicabilitySignatureIdentifier
        case "HybridTaskIdentifier" => ArcHybridTaskIdentifier
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        stateRefPropertyIdentifierFormat,
        taskConjunctionIdentifierFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
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
      activatedTaskCounts = activatedTaskCounts.updated(task, 1 + activatedTaskCounts.getOrElse(task, 0))
    }
    val finalTasks: Set[ClassificationTask] = ((activatedTaskCounts filter
      {
        case (task, count) =>
          (count >= activationThreshold)
      }).keySet) union Set(TaskConjunction(Seq()))

    TaskConjunctionIdentifier(taskIdentifiers, Some(finalTasks))
  }
}
