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

    def write(task: ClassificationTask): JsValue = task match {
      case appSignature: ApplicabilitySignature => appSignature.toJson
      case stateRefProperty: StateRefProperty => stateRefProperty.toJson
      case taskConjunction: TaskConjunction => taskConjunction.toJson
    }

    def read(value: JsValue): ClassificationTask = value.asJsObject.unpackWith(
      applicabilitySignatureFormat,
      stateRefPropertyFormat,
      taskConjunctionFormat)
  }
}

/** The TaskConjunction is a conjunction of ClassificationTasks.
  *
  * @param tasks the tasks we want to conjoin
  */
case class TaskConjunction(val tasks: Seq[ClassificationTask]) extends ClassificationTask {

  /** Returns an identifier that can be used in a filename. */
  @transient
  override val filenameFriendlyName: String = {
    if(tasks.nonEmpty) {
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
      jsonFormat1(TaskConjunctionIdentifier.apply).pack("type" -> "TaskConjunctionIdentifier")

    implicit val taskTreeIdentifierFormat =
      jsonFormat1(TaskTreeIdentifier.apply).pack("type" -> "TaskTreeIdentifier")

    def write(taskIdentifier: TaskIdentifier): JsValue = taskIdentifier match {
      case ApplicabilitySignatureIdentifier => JsString("ApplicabilitySignatureIdentifier")
      case stateRefPropertyIdentifier: StateRefPropertyIdentifier =>
        stateRefPropertyIdentifier.toJson
      case taskIdentifier: TaskConjunctionIdentifier => taskIdentifier.toJson
      case treeIdentifier: TaskTreeIdentifier => treeIdentifier.toJson
    }

    def read(value: JsValue): TaskIdentifier = value match {
      case JsString(typeid) => typeid match {
        case "ApplicabilitySignatureIdentifier" => ApplicabilitySignatureIdentifier
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(stateRefPropertyIdentifierFormat,
        taskConjunctionIdentifierFormat,
        taskTreeIdentifierFormat)
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** The TaskConjunctionIdentifier allows you to create a TaskIdentifier by conjoining
  * existing TaskIdentifiers. You will want to do this if you want to partition feature vectors
  * according to multiple criteria at once.
  *
  * @param taskIdentifiers the task identifiers you want to conjoin
  */
case class TaskConjunctionIdentifier(
    taskIdentifiers: List[TaskIdentifier]) extends TaskIdentifier {

  override def apply(state: State): Option[ClassificationTask] = {
    val optTasks: Seq[Option[ClassificationTask]] = taskIdentifiers map {
      case taskIdentifier =>
        taskIdentifier(state)
    }
    if (optTasks.contains(None)) {
      None
    } else {
      Some(TaskConjunction(optTasks map { optTask => optTask.get }))
    }
  }
}

/** A TaskTree can be viewed as a tree-structured TaskConjunctionIdentifier. Recall that a
  * TaskConjunctionIdentifier associates every TransitionParserState with a TaskConjunction.
  *
  * To do this, each TaskTree is associated with an optional TaskIdentifier ident. If this is None,
  * then it will associate every state with the trivial TaskConjunction
  * (i.e. TaskConjunction(List())). Otherwise, it will compute the ClassificationTask returned
  * by applying the TaskIdentifier to the state. If this task is not contained in its children
  * map, then it will associate the state with TaskConjunction(List(ident)). Otherwise it will
  * recursive call the child TaskTree on the state, and accumulate a TaskConjunction.
  *
  * @param baseIdentifier the (optional) TaskIdentifier associated with this tree
  * @param children a mapping from ClassificationTasks (in the range of `baseIdentifier`) to
  *           TaskTrees
  */
case class TaskTree(baseIdentifier: Option[TaskIdentifier],
    children: List[(ClassificationTask, TaskTree)]) {

  @transient
  val childrenMap = children.toMap

  /** Identifies the TaskConjunction associated with the argument state.
    *
    * @param state the parser state of interest
    * @return the TaskConjunction associated with the argument state
    */
  def identifyTaskConjunction(state: State): TaskConjunction = {
    TaskConjunction(getTaskList(state, Seq()).toList)
  }

  /** Helper function of .identifyTaskConjunction.
    *
    * @param state the parser state of interest for .identifyTaskConjunction
    * @return the constructor argument for TaskConjunction
    * ß    *
    * TODO: double-check correctness
    */
  @tailrec
  private final def getTaskList(state: State,
    tasksSoFar: Seq[ClassificationTask]): Seq[ClassificationTask] = {

    baseIdentifier match {
      case None => tasksSoFar
      case Some(baseId) => {
        val rootTask: Option[ClassificationTask] = baseId(state)
        rootTask match {
          case Some(task) =>
            childrenMap.get(task) match {
              case None => tasksSoFar
              case Some(child) => child.getTaskList(state, tasksSoFar :+ task)
            }
          case None => tasksSoFar
        }
      }
    }
  }
}

object TaskTree {
  implicit val taskTreeFormat: JsonFormat[TaskTree] =
    lazyFormat(jsonFormat(TaskTree.apply, "baseIdentifier", "children"))

  def learnTaskTree(taskIdentifiers: IndexedSeq[TaskIdentifier],
    goldStates: Iterator[State],
    qualifyingCount: Int): TaskTree = {
    // get task tree node histogram: each gold parser state corresponds to a node of the full task
    // tree; in the following lines, we want to count how many parser states correspond to each
    // task tree node
    type TaskTreeNode = List[ClassificationTask]
    val reversedTaskIdentifiers = taskIdentifiers.reverse
    val taskConjunctionIdentifiers = (0 to reversedTaskIdentifiers.size - 1) map
      { i => reversedTaskIdentifiers.drop(i) }
    val taskTreeNodes: Iterator[TaskTreeNode] = (for {
      parserState <- goldStates
      taskIdentifierSuffix <- taskConjunctionIdentifiers
    } yield (taskIdentifierSuffix map { taskIdentifier => taskIdentifier(parserState).get }).toList)
    val taskTreeNodeCounts: Map[TaskTreeNode, Int] = taskTreeNodes.toList groupBy
      { x => x } mapValues { x => x.size }

    // now we filter out the task tree nodes that do not have a qualifying number of
    // corresponding gold states
    val filteredTaskTreeNodes: Iterable[TaskTreeNode] = (taskTreeNodeCounts filter
      { case (task, count) => (task.size < 0 || count >= qualifyingCount) }).keys

    // next we design the structure of the task tree by determining the children nodes of each
    // surviving task tree node
    val originalTaskChildren = (filteredTaskTreeNodes map { task => (task, List()) }).toMap
    val taskChildren: Map[TaskTreeNode, Iterable[TaskTreeNode]] =
      originalTaskChildren ++ ((filteredTaskTreeNodes map (task => (task, task.tail)))
        groupBy { _._2 }) map { case (key, value) => (key, value.unzip._1) }

    // finally we construct the task tree by constructing the nodes in reverse topological
    // order
    val sortedTaskChildren = taskChildren.toList sortBy { case (task, children) => -task.size }
    var taskTrees = mutable.Map[TaskTreeNode, TaskTree]()
    val taskIdentifierMap = (reversedTaskIdentifiers.size - 1 to 0 by -1).zip(
      reversedTaskIdentifiers).toMap
    for ((task, children) <- sortedTaskChildren) {
      taskTrees += (task ->
        TaskTree(taskIdentifierMap.get(task.length),
          (children map { child => (child.head, taskTrees(child)) }).toList))
    }

    taskTrees.getOrElse(List(), new TaskTree(None, List()))
  }
}

/** This is a wrapper for TaskTree that implements the TaskIdentifier interface.
  *
  * It exists mainly for ease of serialization. See the TaskTree documentation for more details
  * on its functionality.
  *
  * @param taskTree the task tree that drives this TaskIdentifier
  */
case class TaskTreeIdentifier(taskTree: TaskTree) extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    Some(taskTree.identifyTaskConjunction(state))
  }
}

object TaskTreeIdentifier {

  /** Learn a TaskTreeIdentifier from a list of TaskIdentifiers. It will enumerate the set of
    * parser states found in a set of gold parses. Then, from the set of possible TaskConjunctions
    * induced by the TaskIdentifier list, it will compute the subset which are associated with
    * at least `qualifyingCount` parser states. Then it will build a tree-structure over these
    * and return a TaskTreeIdentifier.
    *
    * @param taskIdentifiers a list of base TaskIdentifiers
    * @param goldStates a set of gold states
    * @param qualifyingCount the minimum state count that a TaskConjunction needs to qualify
    *                     for the resulting TaskTree
    * @return the resulting TaskTreeIdentifier
    */
  def learnTaskTreeIdentifier(taskIdentifiers: IndexedSeq[TaskIdentifier],
    goldStates: Iterator[State],
    qualifyingCount: Int): TaskTreeIdentifier = {

    TaskTreeIdentifier(TaskTree.learnTaskTree(taskIdentifiers, goldStates, qualifyingCount))
  }
}
