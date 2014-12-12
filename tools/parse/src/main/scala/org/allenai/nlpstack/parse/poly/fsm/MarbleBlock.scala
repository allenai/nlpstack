package org.allenai.nlpstack.parse.poly.fsm

/** A MarbleBlock is an unstructured input corresponding to a start state of a finite-state
  * machine. The goal of the finite-state machine is to find a final state (which correponds
  * to a Sculpture, i.e. a structured output).
  *
  * As an example, consider a transition-based parser. A MarbleBlock would be a sentence to be
  * parsed, whereas a Sculpture would be a parse tree for that sentence.
  */
trait MarbleBlock
