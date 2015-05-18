package org.allenai.nlpstack.parse.poly.core

import reming.DefaultJsonProtocol._

/** A Token is the basic atom of a sentence.
  *
  * @param word the surface form of the token
  */
case class Token(word: Symbol, properties: Map[Symbol, Set[Symbol]] = Map()) {
  def getProperty(propertyName: Symbol): Set[Symbol] = {
    properties.getOrElse(propertyName, Set())
  }

  def getDeterministicProperty(propertyName: Symbol): Symbol = {
    val propertyValues: Set[Symbol] = getProperty(propertyName)
    require(propertyValues.size <= 1, ".getDeterministicProperty cannot be called on " +
      s"nondeterministic property ${propertyName}")
    propertyValues.headOption match {
      case Some(value) => value
      case _ => Token.propertyNotFound
    }
  }

  def updateProperties(moreProperties: Map[Symbol, Set[Symbol]]): Token = {
    Token(word, properties ++ moreProperties)
  }

  /** Returns true if this token is considered punctuation.
    *
    * Note that "." is the Google coarse part-of-speech tag for all punctuation.
    *
    * @return true if this token is considered punctuation
    */
  def isPunctuation: Boolean = {
    getDeterministicProperty('cpos) == Symbol(".")
  }
}

object Token {
  implicit val tokenJsonFormat = jsonFormat2(Token.apply)

  def createProperties(word: String, goldCpos: Option[String] = None): Map[Symbol, Set[Symbol]] = {
    val propertyMap = Map('lcase -> Set(Symbol(word.toLowerCase)))
    goldCpos match {
      case Some(cpos) => propertyMap.updated('cpos, Set(Symbol(cpos)))
      case None => propertyMap
    }
  }

  def create(word: String, coarsePos: String): Token = {
    Token(Symbol(word), createProperties(word, Some(coarsePos)))
  }

  val propertyNotFound = 'notFound
}

/** The NexusToken is the "zeroth" token of a dependency parse. */
object NexusToken extends Token('nexus, Token.createProperties("nexus", Some("nexus")))
