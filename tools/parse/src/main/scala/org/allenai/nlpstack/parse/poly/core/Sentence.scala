package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.immutable.Interval
import org.allenai.nlpstack.parse.poly.fsm.MarbleBlock
import org.allenai.nlpstack.parse.poly.ml.FeatureVector

import reming.DefaultJsonProtocol._

import scala.collection.mutable.Stack

/** A Sentence is a sequence of tokens.
  *
  * @param tokens the sequence of tokens in the sentence
  */
case class Sentence(tokens: IndexedSeq[Token]) extends MarbleBlock {

  @transient lazy val asWhitespaceSeparatedString =
    (tokens.tail map { tok => tok.word.name }).mkString(" ")

  /** The number of tokens in the sentence (including the nexus). */
  @transient val size: Int = tokens.size

  /** A set of Intervals representing offsets for a group of tokens in sentence that
    * are part of a parenthesized chunk (including the parens themselves).
    * For use in IsBracketedTransform.
    */
  @transient lazy val parenIntervals: Set[Interval] = {

    // Result seq to return
    var parenIntervals = Set.empty[Interval]
    // Stack for book-keeping parens
    val parenStack = new Stack[Int]

    // Process each token, get its index and process appropriately,
    // checking for parens.
    for ((token, ix) <- tokens.zipWithIndex) {
      token.word.name match {
        case "(" =>
          {
            parenStack.push(ix)
          }
        case ")" =>
          {
            // If a left paren was encountered before this right paren, pop
            // out the latest encountered left paren and form an Interval
            // from it to the current right paren. If no left paren was encountered
            // before this right paren, this is possibly a case of multiple sentences
            // within parentheses. Form an Interval from the beginning of the sentence
            // to the right paren.
            if (parenStack.length > 0) {
              val startIx = parenStack.pop
              parenIntervals = parenIntervals + Interval.closed(startIx, ix)
            } else {
              parenIntervals = parenIntervals + Interval.closed(0, ix)
            }
          }
        case _ =>
      }
    }
    // If a matching close paren was not found, this is possibly a case of
    // multiple sentences occurring in parentheses. Create an Interval
    // from the first-encountered open paren among the pending open parens
    // (bottom of stack) to the end of the sentence.
    var unmatchedParenIx = -1
    while (parenStack.length > 0) {
      unmatchedParenIx = parenStack.pop()
    }
    if (unmatchedParenIx > -1) {
      parenIntervals = parenIntervals + Interval.closed(unmatchedParenIx, tokens.size - 1)
    }

    parenIntervals
  }
}

object Sentence {
  implicit val sentenceJsonFormat = jsonFormat1(Sentence.apply)

  /** Initializes a Sentence object from a string, by splitting on whitespace to create tokens.
    *
    * Note that the zeroth token of the initialized sentence will be a NexusToken.
    *
    * @param rawString the input string
    * @return the corresponding Sentence object
    */
  def initializeFromWhitespaceSeparatedString(rawString: String): Sentence = {
    val tokens = rawString.trim.split("\\s+") map { str => Token(Symbol(str)) }
    Sentence(NexusToken +: tokens.toIndexedSeq)
  }
}

/** A data source for Sentence objects. */
trait SentenceSource {
  def sentenceIterator: Iterator[Sentence]
}

/** An AnnotatedSentence is a sentence whose tokens are each annotated with a feature
  * vector.
  *
  * @param sentence the unannotated sentence
  * @param annotation an indexed sequence, of which the nth element is the feature vector for
  * the nth token of the sentence
  */
case class AnnotatedSentence(sentence: Sentence, annotation: IndexedSeq[FeatureVector])

object AnnotatedSentence {
  implicit val annotatedSentenceJsonFormat = jsonFormat2(AnnotatedSentence.apply)
}

