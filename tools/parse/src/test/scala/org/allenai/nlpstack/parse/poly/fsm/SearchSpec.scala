package org.allenai.nlpstack.parse.poly.fsm
// scalastyle:off

case class AlphabetBlock(letter: Symbol) extends State with MarbleBlock with Sculpture {
  override val marbleBlock: MarbleBlock = this
  override val isFinal: Boolean = (letter == 'H)
  override def asSculpture: Option[Sculpture] = Some(this)
}

case object Flip extends StateTransition {
  override def apply(state: Option[State]): Option[State] = {
    state flatMap { actualState =>
      actualState match {
        case AlphabetBlock(letter) =>
          Some(AlphabetBlock(flippingMap(letter)))
        case _ =>
          None
      }
    }
  }

  override val name: String = "Flip"

  private val flippingMap = Map('A -> 'H, 'B -> 'G, 'C -> 'F, 'D -> 'E,
    'E -> 'D, 'F -> 'C, 'G -> 'B, 'H -> 'A)
}

case object Forward extends StateTransition {
  override def apply(state: Option[State]): Option[State] = {
    state flatMap { actualState =>
      actualState match {
        case AlphabetBlock(letter) =>
          Some(AlphabetBlock(forwardMap(letter)))
        case _ =>
          None
      }
    }
  }

  override val name: String = "Forward"

  private val forwardMap = Map('A -> 'B, 'B -> 'C, 'C -> 'D, 'D -> 'E,
    'E -> 'F, 'F -> 'G, 'G -> 'H, 'H -> 'A)
}

case object Backward extends StateTransition {
  override def apply(state: Option[State]): Option[State] = {
    state flatMap { actualState =>
      actualState match {
        case AlphabetBlock(letter) =>
          Some(AlphabetBlock(backwardMap(letter)))
        case _ =>
          None
      }
    }
  }

  override val name: String = "Backward"

  private val backwardMap = Map('A -> 'H, 'B -> 'A, 'C -> 'B, 'D -> 'C,
    'E -> 'D, 'F -> 'E, 'G -> 'F, 'H -> 'G)
}

case class AlphabetBlockTransitionSystem(marbleBlock: MarbleBlock) extends TransitionSystem {
  override def initialState(
    constraints: Seq[TransitionConstraint]
  ): Option[State] = {

    marbleBlock match {
      case block: AlphabetBlock => Some(block)
      case _ => None
    }
  }

  override val taskIdentifier: TaskIdentifier = new SimpleTaskIdentifier("alpha")

  def guidedCostFunction(goldObj: Sculpture): Option[StateCostFunction] = None

  private val feature: StateFeature = FeatureUnion(Seq())

  def computeFeature(state: State) = feature(state)

  def toSculpture(state: State): Option[Sculpture] = {
    state match {
      case block: AlphabetBlock => Some(block)
      case _ => None
    }
  }

  def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {
    TransitionSystem.trivialConstraint
  }
}

case class AlphabetBlockCostFunction1(transitionSystem: TransitionSystem)
    extends StateCostFunction {

  override def apply(state: State): Map[StateTransition, Double] = {
    Map(Forward -> 1, Backward -> 2, Flip -> 3)
  }
}

