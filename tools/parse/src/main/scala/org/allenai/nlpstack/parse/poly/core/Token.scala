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

  def updateProperty(propertyKey: Symbol, propertyValue: Set[Symbol]): Token = {
    Token(word, properties ++ Map(propertyKey -> propertyValue))
  }

  def updateProperties(moreProperties: Map[Symbol, Set[Symbol]]): Token = {
    Token(word, properties ++ moreProperties)
  }
}

object Token {
  implicit val tokenJsonFormat = jsonFormat2(Token.apply)

  def createDefaultProperties(word: String, goldCpos: Option[String],
    goldPos: Option[String]): Map[Symbol, Set[Symbol]] = {

    //val propertyMap = Map('lcase -> Set(Symbol(word.toLowerCase)))
    val propertyMap = Map[Symbol, Set[Symbol]]()
    val revisedMap = goldCpos match {
      case Some(cpos) => propertyMap.updated('cpos, Set(Symbol(cpos)))
      case None => propertyMap
    }
    goldPos match {
      case Some(pos) => revisedMap.updated('pos, Set(Symbol(pos)))
      case None => revisedMap
    }
  }

  def create(word: String, coarsePos: Option[String], finePos: Option[String]): Token = {
    Token(Symbol(word), createDefaultProperties(word, coarsePos, finePos))
  }

  val propertyNotFound = 'notFound
}

/** The NexusToken is the "zeroth" token of a dependency parse. */
object NexusToken
  extends Token('nexxx, Token.createDefaultProperties("nexxx", None, None))
