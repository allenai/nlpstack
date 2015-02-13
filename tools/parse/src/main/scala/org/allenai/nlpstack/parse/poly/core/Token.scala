package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.immutable.Interval
import org.allenai.nlpstack.core.{ PostaggedToken, Tokenizer, Token => NLPStackToken }
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.parse.poly.fsm.MarbleBlock
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, FeatureName, BrownClusters }
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable.Stack

/** A Token is the basic atom of a sentence.
  *
  * @param word the surface form of the token
  */
case class Token(val word: Symbol, properties: Map[Symbol, Set[Symbol]] = Map()) {
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
}

object Token {
  implicit val tokenJsonFormat = jsonFormat2(Token.apply)

  def createProperties1(word: String, goldCpos: Option[String] = None): Map[Symbol, Set[Symbol]] = {
    val propertyMap = Map('lcase -> Set(Symbol(word.toLowerCase)))
    goldCpos match {
      case Some(cpos) => propertyMap.updated('cpos, Set(Symbol(cpos)))
      case None => propertyMap
    }
  }

  def create(word: String, coarsePos: String): Token = {
    Token(Symbol(word), createProperties1(word, Some(coarsePos)))
  }

  val propertyNotFound = 'notFound
}

/** The NexusToken is the "zeroth" token of a dependency parse. */
object NexusToken extends Token('nexus, Token.createProperties1("nexus", Some("nexus")))

/** A Sentence is a sequence of tokens.
  *
  * @param tokens the sequence of tokens in the sentence
  */
case class Sentence(tokens: IndexedSeq[Token]) extends MarbleBlock {

  @transient lazy val asWhitespaceSeparatedString =
    (tokens.tail map { tok => tok.word.name }).mkString(" ")

  /** The number of tokens in the sentence (including the nexus). */
  @transient val size: Int = tokens.size

  /** For each token, "look ahead" and aggregrate the property values for later tokens. For
    * instance, lookAhead(3)('cpos) gives the set of all values of the "cpos" property for
    * tokens after (and not including) token 3.
    */
  /*
  @transient lazy val lookAhead: IndexedSeq[Map[Symbol, Set[String]]] = {
    val properties: Seq[Symbol] = (tokens map { _.properties } map { _.keys.toSet } reduce {
      (x, y) => x ++ y
    }).toSeq
    val valuesByProperty: Seq[Seq[Set[String]]] = properties map { property =>
      tokens map { tok =>
        tok.properties.get(property) match {
          case Some(x) => Set(x)
          case None => Set[String]()
        }
      }
    }
    val lookAheadByProperty: Seq[IndexedSeq[Set[String]]] = valuesByProperty map { property =>
      Range(0, property.size) map { x =>
        { (property.drop(x + 1) :+ Set[String]()) reduce { (s1, s2) => s1 ++ s2 } }
      }
    }
    val result = ((properties zip lookAheadByProperty) map {
      case (property, valuesList) =>
        (valuesList map { value => Map(property -> value) })
    }).transpose map { mapList => mapList reduce { (x, y) => x ++ y } }
    result.toIndexedSeq
  }
  */
  /** For each token, "look behind" and aggregrate the property values for earlier tokens. For
    * instance, lookBehind(3)('cpos) gives the set of all values of the "cpos" property for
    * tokens before (and not including) token 3.
    */
  /*
  @transient lazy val lookBehind: IndexedSeq[Map[Symbol, Set[String]]] = {
    val properties: Seq[Symbol] = (tokens map { _.properties } map { _.keys.toSet } reduce {
      (x, y) => x ++ y
    }).toSeq
    val valuesByProperty: Seq[Seq[Set[String]]] = properties map { property =>
      tokens map { tok =>
        tok.properties.get(property) match {
          case Some(x) => Set(x)
          case None => Set[String]()
        }
      }
    }
    val lookAheadByProperty: Seq[IndexedSeq[Set[String]]] = valuesByProperty map { property =>
      Range(0, property.size) map { x =>
        { (property.take(x) :+ Set[String]()) reduce { (s1, s2) => s1 ++ s2 } }
      }
    }
    val result = ((properties zip lookAheadByProperty) map {
      case (property, valuesList) =>
        (valuesList map { value => Map(property -> value) })
    }).transpose map { mapList => mapList reduce { (x, y) => x ++ y } }
    result.toIndexedSeq
  }
  */

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
      unmatchedParenIx = parenStack.pop
    }
    if (unmatchedParenIx > -1) {
      parenIntervals = parenIntervals + Interval.closed(unmatchedParenIx, tokens.size - 1)
    }

    parenIntervals
  }

  @transient
  lazy val taggedWithFactorie: Sentence = {
    val words: IndexedSeq[String] = tokens.tail map { tok => tok.word.name }
    val nlpStackTokens: IndexedSeq[NLPStackToken] =
      Tokenizer.computeOffsets(words, words.mkString).toIndexedSeq
    val taggedTokens: IndexedSeq[PostaggedToken] =
      defaultPostagger.postagTokenized(nlpStackTokens).toIndexedSeq
    Sentence(NexusToken +: (taggedTokens.zip(tokens.tail) map {
      case (tagged, untagged) =>
        untagged.updateProperties(Map(
          'factoriePos -> Set(Symbol(tagged.postag)),
          'factorieCpos -> Set(Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(
            tagged.postag, "X"
          )))
        ))
      //Token(untagged.word, untagged.properties.updated('factoriePos, Set(Symbol(tagged.postag)))
      //  .updated('factorieCpos,
      //    Set(Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(tagged.postag, "X")))))
    }))
  }

  @transient def taggedWithLexicalProperties: Sentence = {
    Sentence(tokens map { tok =>
      val tokStr = tok.word.name
      val firstLetterCapital = tokStr.headOption match {
        case Some(x) if Character.isUpperCase(x) => Set('firstCap)
        case _ => Set[Symbol]()
      }
      val existsCapital = tokStr match {
        case tokStr: String if tokStr exists { Character.isUpperCase(_) } => Set('existsCap)
        case _ => Set[Symbol]()
      }
      val allCaps = tokStr match {
        case tokStr: String if tokStr forall { Character.isUpperCase(_) } => Set('allCaps)
        case _ => Set[Symbol]()
      }
      val existsNumber = tokStr match {
        case tokStr: String if tokStr exists { Character.isDigit(_) } => Set('existsNum)
        case _ => Set[Symbol]()
      }
      tok.updateProperties(Map(
        'lexical -> (firstLetterCapital ++ existsCapital ++ allCaps ++ existsNumber)
      ))
    })
  }

  @transient def taggedWithBrownClusters(clusters: Seq[BrownClusters]): Sentence = {
    Sentence(for {
      tok <- tokens
    } yield tok.updateProperties((for {
      (cluster, clusterId) <- clusters.zipWithIndex
    } yield {
      Symbol(s"brown${clusterId}") ->
        Set(cluster.getMostSpecificCluster(Symbol(tok.word.name.toLowerCase)))
    }).toMap))
  }
}

object Sentence {
  implicit val sentenceJsonFormat = jsonFormat1(Sentence.apply)
}

case class AnnotatedSentence(sentence: Sentence, annotation: IndexedSeq[FeatureVector])

object AnnotatedSentence {
  implicit val annotatedSentenceJsonFormat = jsonFormat2(AnnotatedSentence.apply)
}

/** A data source for Sentence object. */
trait SentenceSource {
  def sentenceIterator: Iterator[Sentence]
}
