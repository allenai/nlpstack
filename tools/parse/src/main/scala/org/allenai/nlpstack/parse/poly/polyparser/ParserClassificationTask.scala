package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm._

/** The StateRefProperty is a ClassificationTask for which we are trying to predict the next
  * transition, given that we know some property of a particular token of the parser state.
  */
case class StateRefProperty(val stateRef: StateRef, val property: Symbol,
  val propertyValue: String)
    extends ClassificationTask {

  /** Returns an identifier that can be used in a filename. */
  @transient
  override val filenameFriendlyName: String = {
    s"stateRefProperty_${stateRef.name}_${property.name}_${propertyValue}"
  }
}

/** The StateRefPropertyIdentifier identifies the ClassificationTask of a parser state according
  * to the coarse part-of-speech tag of a particular word of the state (as identified by a
  * StateRef).
  */
case class StateRefPropertyIdentifier(stateRef: StateRef,
    property: Symbol) extends TaskIdentifier {

  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case tpState: TransitionParserState =>
        Some(stateRef(tpState).headOption match {
          case Some(index) =>
            val cpos = tpState.sentence.tokens(index).getDeterministicProperty(property)
            StateRefProperty(stateRef, property, cpos.name)
          case None => StateRefProperty(stateRef, property, "noTokenHere")
        })
      case _ => None
    }
  }
}


/** The ApplicabilitySignature is a ClassificationTask for which we are trying to predict
  * the next transition, given that only a subset of possible transitions are applicable.
  *
  * If we choose this as our ClassificationTask, we will train separate classifiers for parser
  * states that have different ApplicabilitySignatures.
  *
  * @param shift true iff Shift is applicable
  * @param reduce true iff Reduce is applicable
  * @param left true iff LeftArc and InvertedLeftArc are both applicable (for any labeling)
  * @param right true iff RightArc and InvertedRightArc are both applicable (for any labeling)
  */
case class ApplicabilitySignature(val shift: Boolean, val reduce: Boolean, val left: Boolean,
    val right: Boolean) extends ClassificationTask {

  /** Returns an identifier that can be used in a filename. */
  @transient
  override val filenameFriendlyName: String = {
    "appl" + (List(shift, reduce, left, right) map {
      case true => 1
      case _ => 0
    }).mkString
  }
}

/** The ApplicabilitySignatureIdentifier identifies the ClassificationTask of a parser state
  * according to the state's applicability signature.
  */
object ApplicabilitySignatureIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    Some(ApplicabilitySignature(StateTransition.applicable(ArcEagerShift, Some(state)),
      StateTransition.applicable(ArcEagerReduce, Some(state)),
      ArcEagerLeftArc.applicable(state), ArcEagerRightArc.applicable(state)))
  }
}
