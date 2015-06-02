package org.allenai.nlpstack.parse.poly.core

import reming.DefaultJsonProtocol._

/** A Token is the basic atom of a sentence, typically corresponding to a word.
  *
  * Each Token is associated with a symbol-to-symbolset properties map.
  *
  * @param word the surface form of the token (i.e. the word)
  * @param properties a symbol-to-symbolset map, used for annotating the token with properties
  */
case class Token(word: Symbol, properties: Map[Symbol, Set[Symbol]] = Map()) {

  /** Retrieves the value of a specified property.
    *
    * If the property is not defined, then the empty set is returned.
    *
    * @param propertyName the desired property
    * @return the value of the desired property (a set of symbols)
    */
  def getProperty(propertyName: Symbol): Set[Symbol] = {
    properties.getOrElse(propertyName, Set())
  }

  /** For singleton properties (i.e. the value is a singleton set), this returns the single
    * symbol in that set.
    *
    * For undefined properties, a reserved Token.propertyNotFound symbol is returned.
    *
    * An exception is thrown if the specified property is associated with a non-singleton set.
    *
    * @param propertyName the desired property
    * @return the singleton value of the desired property
    */
  def getDeterministicProperty(propertyName: Symbol): Symbol = {
    val propertyValues: Set[Symbol] = getProperty(propertyName)
    require(propertyValues.size <= 1, ".getDeterministicProperty cannot be called on " +
      s"nondeterministic property $propertyName")
    propertyValues.headOption match {
      case Some(value) => value
      case _ => Token.propertyNotFound
    }
  }

  /** Replaces the value of the specified property.
    *
    * @param propertyKey desired property to replace
    * @param propertyValue new desired value for the specified property
    * @return a token for which the specified property is mapped to the specified value
    */
  def updateProperty(propertyKey: Symbol, propertyValue: Set[Symbol]): Token = {
    Token(word, properties ++ Map(propertyKey -> propertyValue))
  }

  /** Extends the value of the specified property to include a new symbol.
    *
    * This symbol is added to the current set of symbols that the property maps to.
    *
    * @param propertyKey desired property to extend
    * @param propertyValueExtension new symbol to add to the specified property's value
    * @return a token for which the specified property's value is extended with the specified value
    */
  def extendProperty(propertyKey: Symbol, propertyValueExtension: Symbol): Token = {
    val newValue = getProperty(propertyKey) + propertyValueExtension
    updateProperty(propertyKey, newValue)
  }

  /** Returns true if this token is considered punctuation.
    *
    * Note that "." is the Google coarse part-of-speech tag for all punctuation.
    *
    * @return true if this token is considered punctuation
    */
  def isPunctuation: Boolean = {
    getDeterministicProperty(Token.coarsePos) == Symbol(".")
  }
}

object Token {
  implicit val tokenJsonFormat = jsonFormat2(Token.apply)

  def create(
    word: String,
    coarsePos: Option[String] = None,
    finePos: Option[String] = None
  ): Token = {

    def createDefaultProperties(word: String, goldCpos: Option[String],
      goldPos: Option[String]): Map[Symbol, Set[Symbol]] = {

      val propertyMap = Map[Symbol, Set[Symbol]]()
      val revisedMap = goldCpos match {
        case Some(cpos) => propertyMap.updated(Token.coarsePos, Set(Symbol(cpos)))
        case None => propertyMap
      }
      goldPos match {
        case Some(pos) => revisedMap.updated(Token.finePos, Set(Symbol(pos)))
        case None => revisedMap
      }
    }
    Token(Symbol(word), createDefaultProperties(word, coarsePos, finePos))
  }

  val propertyNotFound = 'notFound
  val coarsePos = 'cpos
  val finePos = 'pos
}

/** The NexusToken is the "zeroth" token of a sentence. */
object NexusToken
  extends Token('nexxx, Map[Symbol, Set[Symbol]]())
