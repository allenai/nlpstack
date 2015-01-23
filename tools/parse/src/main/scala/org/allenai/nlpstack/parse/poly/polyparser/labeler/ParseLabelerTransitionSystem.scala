package org.allenai.nlpstack.parse.poly.polyparser.labeler

import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }
import org.allenai.nlpstack.parse.poly.polyparser.{ ForbiddenArcLabel, PolytreeParse }
import spray.json.DefaultJsonProtocol._

case class ParseLabel(sym: Symbol) {
  override def toString: String = sym.name
}

object ParseLabel {
  implicit val parseLabelJsonFormat = jsonFormat1(ParseLabel.apply)
}

case object ParseLabelerTransitionSystem extends TransitionSystem {
  def initialState(
    marbleBlock: MarbleBlock,
    constraints: Seq[TransitionConstraint]
  ): Option[State] = {

    marbleBlock match {
      case parse: PolytreeParse =>
        Some(ParseLabelerState(parse, Map(), DepthFirstParseNodeSuccessor(parse)(Some(0))))
      case _ => None
    }
  }

  def guidedCostFunction(marbleBlock: MarbleBlock): Option[StateCostFunction] = {
    marbleBlock match {
      case parse: PolytreeParse =>
        Some(new GuidedCostFunctionForLabeler(parse, this))
      case _ => None
    }
  }

  val feature: StateFeature = FeatureUnion(List(BreadcrumbFeature, GrandcrumbFeature,
    NextNodeFeature, RelativeCposFeature, LastWordFeature, SiblingFeature, ChildrenFeature))

  def toSculpture(state: State): Option[Sculpture] = {
    state match {
      case plState: ParseLabelerState =>
        plState.asSculpture
      case _ =>
        None
    }
  }

  override def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {

    constraint match {
      case ForbiddenArcLabel(token1, token2, arcLabel) => {
        (state: State, transition: StateTransition) =>
          state match {
            case plState: ParseLabelerState => transition match {
              case AddNodeLabel(label) =>
                (plState.nextNodeToParse == Some(token1) &&
                  plState.parse.breadcrumb(token1) == token2 &&
                  label.sym == arcLabel) ||
                  (plState.nextNodeToParse == Some(token2) &&
                    plState.parse.breadcrumb(token2) == token1 &&
                    label.sym == arcLabel)
              case _ => false
            }
            case _ => false
          }
      }
      case _ => TransitionSystem.trivialConstraint
    }
  }
}

case class ParseLabelerState(parse: PolytreeParse, labels: Map[Int, ParseLabel],
    nextNodeToParse: Option[Int]) extends State {

  override val isFinal: Boolean = (nextNodeToParse == None)

  // TODO: clean this up (possibly change the PolytreeParse constructor to make more sane).
  def asSculpture: Option[Sculpture] = {
    if (isFinal) {
      val childMap: Map[Int, Set[Int]] = ((parse.breadcrumb.zipWithIndex.tail groupBy { _._1 })
        map { case (key, value) => (key, (value map { _._2 }).toSet) })
      val neighbors: Vector[Set[Int]] = ((0 to (parse.tokens.size - 1)) map { i =>
        childMap.getOrElse(i, Set()) + parse.breadcrumb(i)
      }).toVector
      val arcLabelByTokenPair: Map[Set[Int], Symbol] =
        ((1 to (parse.tokens.size - 1)) map { tok: Int =>
          ((Set(tok, parse.breadcrumb(tok))), Symbol(labels(tok).toString))
        }).toMap
      val arcLabels: Vector[Set[(Int, Symbol)]] = for {
        (neighborSet, i) <- neighbors.zipWithIndex
      } yield for {
        neighbor <- neighborSet
        if neighbor >= 0
      } yield (neighbor, arcLabelByTokenPair(Set(i, neighbor)))
      Some(PolytreeParse(parse.sentence, parse.breadcrumb, parse.children, arcLabels))
    } else {
      None
    }
  }
}

case class AddNodeLabel(label: ParseLabel) extends StateTransition {
  override def apply(state: Option[State]): Option[State] = {
    state match {
      case Some(labelerState: ParseLabelerState) =>
        labelerState.nextNodeToParse map { node =>
          ParseLabelerState(
            labelerState.parse,
            labelerState.labels.updated(node, label),
            DepthFirstParseNodeSuccessor(labelerState.parse)(labelerState.nextNodeToParse)
          )
        }
      case _ => None
    }
  }

  @transient
  override val name: String = s"Label[${label}]"
}

class GuidedCostFunctionForLabeler(
    goldParse: PolytreeParse,
    override val transitionSystem: TransitionSystem
) extends StateCostFunction {

  override def apply(state: State): Map[StateTransition, Double] = {
    state match {
      case labelerState: ParseLabelerState =>
        val goldLabel: Option[ParseLabel] = labelerState.nextNodeToParse map { node =>
          ParseLabel(goldParse.breadcrumbArcLabel(node))
        }
        (goldLabel map { x => (AddNodeLabel(x), 0.0) }).toMap
      case _ => Map()
    }
  }
}

trait ParseNodeSuccessor extends (Option[Int] => Option[Int])

case class DepthFirstParseNodeSuccessor(parse: PolytreeParse) extends ParseNodeSuccessor {

  private val successors: Map[Option[Int], Option[Int]] =
    ((parse.depthFirstPreorder map { x => Some(x) }).zipAll(
      parse.depthFirstPreorder.tail map { x => Some(x) }, None, None
    )).toMap

  override def apply(nodeIndex: Option[Int]): Option[Int] = {
    successors.getOrElse(nodeIndex, None)
  }
}

/*
case object BreadcrumbLabelFeature extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case plState: ParseLabelerState =>
        FeatureVector(plState.nextNodeToParse match {
          case Some(nodeIndex) =>
            Map(getFeatureName(plState.labels.get(plState.parse.breadcrumb(nodeIndex))) -> 1.0)
          case None =>
            Map()
        })
      case _ =>
        FeatureVector(Map())
    }
  }

  def getFeatureName(label: Option[ParseLabel]): FeatureName = {
    FeatureName(List('breadcrumbLabel, label match {
      case Some(x) => Symbol(x.toString)
      case None => 'None
    }))
  }
}
*/

abstract class ParseLabelerStateFeature extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case plState: ParseLabelerState =>
        FeatureVector(plState.nextNodeToParse match {
          case Some(nodeIndex) =>
            applyHelper(plState, nodeIndex)
          case None =>
            Seq()
        })
      case _ =>
        FeatureVector(Seq())
    }
  }

  def applyHelper(state: ParseLabelerState, nextNodeToParse: Int): Seq[(FeatureName, Double)]
}

case object SiblingFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    state.parse.siblings(nextNodeToParse).toSeq flatMap { sibling =>
      getMappings(state, sibling)
    }
  }

  def getMappings(state: ParseLabelerState, sibling: Int): List[(FeatureName, Double)] = {
    val token = state.parse.tokens(sibling)
    List(
      FeatureName(List('siblingPos, token.getDeterministicProperty('factoriePos)))
        -> 1.0,
      FeatureName(List('siblingCpos, token.getDeterministicProperty('cpos)))
        -> 1.0,
      FeatureName(List('siblingArcLabel, state.labels.get(sibling) match {
        case Some(label) => label.sym
        case _ => 'noArcLabel
      })) -> 1.0
    )
  }
}

case object ChildrenFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    state.parse.gretels.getOrElse(nextNodeToParse, List()) flatMap { child =>
      getMappings(state, child)
    }
  }

  def getMappings(state: ParseLabelerState, child: Int): List[(FeatureName, Double)] = {
    val token = state.parse.tokens(child)
    List(
      FeatureName(List('childPos, token.getDeterministicProperty('factoriePos)))
        -> 1.0,
      FeatureName(List('childCpos, token.getDeterministicProperty('cpos)))
        -> 1.0
    )
  }
}

case object NextNodeFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    val token = state.parse.tokens(nextNodeToParse)
    Seq(
      FeatureName(List('nextNodePos, token.getDeterministicProperty('factoriePos)))
        -> 1.0,
      FeatureName(List('nextNodeCpos, token.getDeterministicProperty('cpos)))
        -> 1.0
    )
  }
}

case object BreadcrumbFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    val breadcrumb: Int = state.parse.breadcrumb(nextNodeToParse)
    if (breadcrumb < 0) {
      Seq(
        FeatureName(List('noBreadcrumb)) -> 1.0
      )
    } else {
      val token = state.parse.tokens(breadcrumb)
      Seq(
        FeatureName(List('crumbArcLabel, state.labels.get(breadcrumb) match {
          case Some(label) => label.sym
          case _ => 'noCrumbLabel
        })) -> 1.0,
        FeatureName(List('crumbPos, token.getDeterministicProperty('factoriePos)))
          -> 1.0,
        FeatureName(List('crumbCpos, token.getDeterministicProperty('cpos)))
          -> 1.0
      )
    }
  }
}

case object GrandcrumbFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    val grandcrumb: Int =
      if (state.parse.breadcrumb(nextNodeToParse) >= 0) {
        state.parse.breadcrumb(state.parse.breadcrumb(nextNodeToParse))
      } else {
        -1
      }
    if (grandcrumb < 0) {
      Seq(
        FeatureName(List('noGrandcrumb)) -> 1.0
      )
    } else {
      val token = state.parse.tokens(grandcrumb)
      Seq(
        FeatureName(List('grandcrumbArcLabel, state.labels.get(grandcrumb) match {
          case Some(label) => label.sym
          case _ => 'noLabel
        })) -> 1.0,
        FeatureName(List('grandcrumbPos, token.getDeterministicProperty('factoriePos)))
          -> 1.0,
        FeatureName(List('grandcrumbCpos, token.getDeterministicProperty('cpos)))
          -> 1.0
      )
    }
  }
}

case object RelativeCposFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    state.parse.relativeCposMap(nextNodeToParse) match {
      case ((isLeft, cposSymbol), count) =>
        Seq(FeatureName(List('relativeCpos, Symbol(isLeft.toString),
          cposSymbol, Symbol(count.toString))) -> 1.0)
    }
  }
}

case object LastWordFeature extends ParseLabelerStateFeature {

  override def applyHelper(
    state: ParseLabelerState,
    nextNodeToParse: Int
  ): Seq[(FeatureName, Double)] = {

    Seq(
      FeatureName(List(
        'lastWordIsQuestionMark,
        Symbol((state.parse.sentence.tokens.last.word == '?).toString)
      )) -> 1.0,
      FeatureName(List(
        'lastWordIsPeriod,
        Symbol((state.parse.sentence.tokens.last.word == Symbol(".")).toString)
      )) -> 1.0
    )
  }
}

/*
case object RelativeCposFeature extends StateFeature {

  override def apply(state: State): FeatureVector = {
    val result = state match {
      case plState: ParseLabelerState =>
        FeatureVector(plState.nextNodeToParse match {
          case Some(nodeIndex) =>
            plState.parse.relativeCposMap(nodeIndex) match {
              case ((isLeft, cposString), count) =>
                Map(FeatureName(List('relativeCpos, Symbol(isLeft.toString),
                  Symbol(cposString), Symbol(count.toString) )) -> 1.0)
            }
          case None =>
            Map()
        })
      case _ =>
        FeatureVector(Map())
    }
    result
  }
}
*/

/*
case object LastWordFeature extends StateFeature {
  override def apply(state: State): FeatureVector = {
    val result = state match {
      case plState: ParseLabelerState =>
        FeatureVector(Map(
          FeatureName(List('lastWordIsQuestionMark,
            Symbol((plState.parse.sentence.tokens.last.word == '?).toString))) -> 1.0,
          FeatureName(List('lastWordIsPeriod,
            Symbol((plState.parse.sentence.tokens.last.word == Symbol(".")).toString))) -> 1.0))
      case _ =>
        FeatureVector(Map())
    }
    result
  }
}
*/

case object RootPathNodeFeature extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case plState: ParseLabelerState =>
        FeatureVector(plState.nextNodeToParse match {
          case Some(nodeIndex) =>
            val rootPath = plState.parse.paths(nodeIndex)
            Seq(FeatureName(List(
              'rootPathNode,
              Symbol((rootPath.tail map { link =>
                plState.labels(link).sym
              }).mkString(">>"))
            )) -> 1.0)
          case None =>
            Seq()
        })
      case _ =>
        FeatureVector(Seq())
    }
  }
}
