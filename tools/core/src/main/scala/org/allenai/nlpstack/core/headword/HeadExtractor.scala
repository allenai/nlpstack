package org.allenai.nlpstack.core.headword

import org.allenai.nlpstack.core.{ Postagger, PostaggedToken, Tokenizer }

trait HeadExtractor {

  /** Given a string representing a relation, will return those
    * tokens comprising the headword(s) of the relation, possibly empty if
    * headword(s) couldn't be determined.
    */
  def relationHead(
    tokenizer: Tokenizer, postagger: Postagger
  )(relation: String): Seq[PostaggedToken]

  /** Given a string representing an argument, will return those
    * tokens comprising the headword(s) of the relation, possibly empty if
    * headword(s) couldn't be determined.
    */
  def argumentHead(
    tokenizer: Tokenizer, postagger: Postagger
  )(argument: String): Seq[PostaggedToken]

  /** Given a Seq[PostaggedToken] representing a relation, will return those
    * tokens comprising the headword(s) of the relation, possibly empty if
    * headword(s) couldn't be determined.
    */
  def relationHead(tokens: Seq[PostaggedToken]): Seq[PostaggedToken]

  /** Given a Seq[PostaggedToken] representing an argument, will return those
    * tokens comprising the headword(s) of the argument, possibly empty if
    * headword(s) couldn't be determined.
    */
  def argumentHead(tokens: Seq[PostaggedToken]): Seq[PostaggedToken]
}
