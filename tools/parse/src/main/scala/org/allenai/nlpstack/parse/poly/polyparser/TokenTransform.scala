package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.json._

import spray.json._
import spray.json.DefaultJsonProtocol._

/** A TokenTransform is a function that maps a token to a set of symbols.
  *
  * The token is described using a TransitionParserState and a StateRef (see the definition of
  * StateRef for details). For instance, using StackRef(0) will cause the TokenTransform to
  * operate on the token at the top of the stack in the current parser state.
  *
  * The purpose of a TokenTransform is primarily to facilitate feature creation (e.g. see
  * StackRefFeature) by allowing us, say for instance, to map the token at top of the state's stack
  * to its word representation. This would be achieved with:
  *
  * WordTransform(state, StackRef(0))
  *
  */
sealed abstract class TokenTransform
    extends ((TransitionParserState, Int) => Set[Symbol]) {

  /** Provides a symbolic representation of the transform, used for creating feature names. */
  def name: Symbol
}

object TokenTransform {

  /** Boilerplate code to serialize a TokenTransform to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM TokenTransform, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object TokenTransformJsonFormat extends RootJsonFormat[TokenTransform] {

    implicit val tokenPropertyTransformFormat =
      jsonFormat1(TokenPropertyTransform.apply).pack("type" -> "TokenPropertyTransform")

    //implicit val lookAheadTransformFormat =
    //  jsonFormat1(LookAheadTransform.apply).pack("type" -> "LookAheadTransform")

    //implicit val lookBehindTransformFormat =
    //  jsonFormat1(LookBehindTransform.apply).pack("type" -> "LookBehindTransform")

    implicit val numChildrenToTheLeftFormat =
      jsonFormat1(NumChildrenToTheLeft.apply).pack("type" -> "NumChildrenToTheLeft")

    implicit val numChildrenToTheRightFormat =
      jsonFormat1(NumChildrenToTheRight.apply).pack("type" -> "NumChildrenToTheRight")

    implicit val keywordTransformFormat =
      jsonFormat1(KeywordTransform.apply).pack("type" -> "KeywordTransform")

    implicit val suffixTransformFormat =
      jsonFormat1(SuffixTransform.apply).pack("type" -> "SuffixTransform")

    implicit val prefixTransformFormat =
      jsonFormat1(PrefixTransform.apply).pack("type" -> "PrefixTransform")

    def write(transform: TokenTransform): JsValue = transform match {
      case WordTransform => JsString("WordTransform")
      case BreadcrumbAssigned => JsString("BreadcrumbAssigned")
      case BreadcrumbArc => JsString("BreadcrumbArc")
      case tp: TokenPropertyTransform => tp.toJson
      //case fp: LookAheadTransform => fp.toJson
      //case bp: LookBehindTransform => bp.toJson
      case lt: NumChildrenToTheLeft => lt.toJson
      case rt: NumChildrenToTheRight => rt.toJson
      case kt: KeywordTransform => kt.toJson
      case st: SuffixTransform => st.toJson
      case pt: PrefixTransform => pt.toJson
      case IsBracketedTransform => JsString("IsBracketedTransform")
    }

    def read(value: JsValue): TokenTransform = value match {
      case JsString(typeid) => typeid match {
        case "WordTransform" => WordTransform
        case "BreadcrumbAssigned" => BreadcrumbAssigned
        case "BreadcrumbArc" => BreadcrumbArc
        case "IsBracketedTransform" => IsBracketedTransform
        case x => deserializationError(s"Invalid identifier for TokenTransform: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        tokenPropertyTransformFormat,
        //lookAheadTransformFormat,
        //lookBehindTransformFormat,
        numChildrenToTheLeftFormat, numChildrenToTheRightFormat, keywordTransformFormat,
        suffixTransformFormat, prefixTransformFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString or JsObject.")
    }
  }
}

/** The WordTransform maps a token to its word representation.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case object WordTransform extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    Set(state.sentence.tokens(tokenIndex).word)
  }

  @transient
  override val name: Symbol = 'wordAt
}

/** The TokenPropertyTransform maps a token to one of its properties.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case class TokenPropertyTransform(property: Symbol)
    extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    state.sentence.tokens(tokenIndex).getProperty(property)
  }

  @transient
  override val name: Symbol = Symbol(property.name + "At")
}

/** The LookAheadTransform maps a token to one of its lookAhead properties. */
/*
case class LookAheadTransform(property: Symbol)
    extends TokenTransform {

  override def apply(state: TransitionParserState, ref: StateRef): Set[Symbol] = {
    ref(state) match {
      case Some(tokenIndex) => state.sentence.lookAhead(tokenIndex).get(property) match {
        case Some(propertyValues) =>
          propertyValues map { x => Symbol(x) }
        case None => Set()
      }
      case None => Set(TokenTransform.noTokenHere)
    }
  }

  @transient
  override val name: Symbol = Symbol(property.name + "AheadAt")
}
*/

/** The LookBehindTransform maps a token to one of its lookBehind properties. */
/*
case class LookBehindTransform(property: Symbol)
    extends TokenTransform {

  override def apply(state: TransitionParserState, ref: StateRef): Set[Symbol] = {
    ref(state) match {
      case Some(tokenIndex) => state.sentence.lookBehind(tokenIndex).get(property) match {
        case Some(propertyValues) =>
          propertyValues map { x => Symbol(x) }
        case None => Set()
      }
      case None => Set(TokenTransform.noTokenHere)
    }
  }

  @transient
  override val name: Symbol = Symbol(property.name + "BehindAt")
}
*/

/** The BreadcrumbAssigned transform maps a token to whether its breadcrumb has been assigned.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case object BreadcrumbAssigned extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    val crumb: Option[Int] = state.breadcrumb.get(tokenIndex)
    Set(crumb match {
      case Some(_) => 'yes
      case None => 'no
    })
  }

  @transient
  override val name: Symbol = 'crumbAssigned
}

case object GuessedCpos extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    state.breadcrumb.get(tokenIndex) match {
      case Some(crumb) =>
        val arcLabel = state.arcLabels.getOrElse(Set(crumb, tokenIndex), Symbol("NEXUS::NEXUS"))
        if (arcLabel != 'NONE) {
          Set(Symbol(arcLabel.name.split("::")(1)))
        } else {
          Set()
        }
      case None => Set()
    }
  }

  @transient
  override val name: Symbol = 'cposGuess
}

case object GuessedArcLabel extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    state.breadcrumb.get(tokenIndex) match {
      case Some(crumb) =>
        val arcLabel = state.arcLabels.getOrElse(Set(crumb, tokenIndex), Symbol("NEXUS::NEXUS"))
        if (arcLabel != 'NONE) {
          Set(Symbol(arcLabel.name.split("::")(0)))
        } else {
          Set()
        }
      case None => Set()
    }
  }

  @transient
  override val name: Symbol = 'arcGuess
}

/** The BreadcrumbArc transform maps a token to the label of the arc from its breadcrumb to itself.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case object BreadcrumbArc extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    state.breadcrumb.get(tokenIndex) match {
      case Some(crumb) =>
        Set(state.arcLabels.getOrElse(Set(crumb, tokenIndex), 'ROOT))
      case None => Set()
    }
  }

  @transient
  override val name: Symbol = 'crumbArc
}

/** The NumChildrenToTheLeft transform maps a token to how many of its children appear to its
  * left in the state's `tokens` sequence.
  *
  * It takes an argument `max` which allows you to specify an upper bound. For instance,
  * if `max` = 3 and a token has 5 children, then applying this transform to that token will return
  * Set(Symbol("3")), not Set(Symbol("5")).
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  * @param max an upper bound on the number of children (anything higher will round down to `max`)
  *
  */
case class NumChildrenToTheLeft(max: Int) extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    Set(state.children.get(tokenIndex) match {
      case Some(childSet: Set[Int]) => {
        val cardinality = childSet count (_ < tokenIndex)
        Symbol(scala.math.min(max, cardinality).toString)
      }
      case None => Symbol("0")
    })
  }

  @transient
  override val name: Symbol = Symbol("leftChildCount" + max)
}

/** The NumChildrenToTheRight transform maps a token to how many of its children appear to its
  * right in the state's `tokens` sequence. This will only be relevant for nodes on the stack
  * (it is impossible for a buffer node to be associated with nodes to its right)
  *
  * It takes an argument `max` which allows you to specify an upper bound. For instance,
  * if `max` = 3 and a token has 5 children to its right, then applying this transform to that
  * token will return Set(Symbol("3")), not Set(Symbol("5")).
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  * @param max an upper bound on the number of children (anything higher will round down to `max`)
  *
  */
case class NumChildrenToTheRight(max: Int) extends TokenTransform {

  //TODO: write unit test
  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    Set(state.children.get(tokenIndex) match {
      case Some(childSet: Set[Int]) => {
        val cardinality = childSet count (_ > tokenIndex)
        Symbol(scala.math.min(max, cardinality).toString)
      }
      case None => Symbol("0")
    })
  }

  @transient
  override val name: Symbol = Symbol("rightChildCount" + max)
}

/** The KeywordTransform maps a token to its word representation, if its word appears in the
  * argument set `keywords`. Otherwise its apply function will return an empty set (if the
  * StateRef points to a valid token) or TokenTransform.noTokenHere (if the StateRef points
  * to an invalid token)
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case class KeywordTransform(keywords: Set[Symbol]) extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    val word = Symbol(state.sentence.tokens(tokenIndex).word.name.toLowerCase)
    if (keywords.contains(word)) {
      Set(word)
    } else {
      Set[Symbol]()
    }
  }

  @transient
  override val name: Symbol = 'keywordAt
}

/** The SuffixTransform maps a token to the set of its suffixes that are contained in a
  * set of "key" suffixes.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  * @param keysuffixes the set of suffixes to treat as "key" suffixes
  *
  */
case class SuffixTransform(keysuffixes: Set[Symbol]) extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    val word = state.sentence.tokens(tokenIndex).word
    keysuffixes filter { suffix => word.name.toLowerCase.endsWith(suffix.name.toLowerCase) }
  }

  @transient
  override val name: Symbol = 'suffixAt
}

/** The PrefixTransform maps a token to the set of its prefixes that are contained in a
  * set of "key" prefixes.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  * @param keyprefixes the set of prefixes to treat as "key" prefixes
  *
  */
case class PrefixTransform(keyprefixes: Set[Symbol]) extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    val word = state.sentence.tokens(tokenIndex).word
    keyprefixes filter { prefix => word.name.toLowerCase.startsWith(prefix.name.toLowerCase) }
  }

  @transient
  override val name: Symbol = 'prefixAt
}

/** The IsBracketedTransform maps a token to a symbol which is 'yes if its word appears
  * between a pair of parentheses, 'no if it is outside of all parentheses pairs,
  * '( if it is a left paren and ') if it is a right paren.
  * It will return a TokenTransform.noTokenHere if the StateRef points
  * to an invalid token.
  *
  * See the definition of TokenTransform (above) for more details about the interface.
  *
  */
case object IsBracketedTransform extends TokenTransform {

  override def apply(state: TransitionParserState, tokenIndex: Int): Set[Symbol] = {
    val token = state.sentence.tokens(tokenIndex)
    val word = token.word.name
    state.sentence.parenIntervals.find(x => x.contains(tokenIndex)) match {
      case Some(_) =>
        {
          word match {
            case "(" => Set(Symbol("("))
            case ")" => Set(Symbol(")"))
            case _ => Set('yes)
          }
        }
      case _ => Set('no)
    }
  }

  @transient
  override val name: Symbol = 'isBracketed
}

object MultiWordTransform extends TokenPropertyTransform(MultiWordTagger.mweSymbol)

