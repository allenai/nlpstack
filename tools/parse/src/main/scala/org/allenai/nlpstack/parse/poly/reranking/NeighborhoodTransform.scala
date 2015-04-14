package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.core.{ Lemmatized, PostaggedToken }
import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, Verbnet }
import org.allenai.nlpstack.parse.poly.polyparser.{ Neighborhood, PolytreeParse }

import reming.DefaultJsonProtocol._

/** A NeighborhoodTransform maps a Neighborhood to zero or more feature names.
  *
  * An example might help. Suppose that we have a neighborhood consisting of
  * (node, child1, child2), i.e. three nodes of a parse tree. A transform might map these
  * to the sequence of their POS tags, e.g. FeatureName(Seq('VERB, 'NOUN, 'NOUN)).
  */
trait NeighborhoodTransform extends ((PolytreeParse, Neighborhood) => Seq[FeatureName])

object NeighborhoodTransform {
  private implicit val arclabelNhTransformJsonFormat = jsonFormat0(() => ArclabelNhTransform)
  private implicit val directionNhTransform = jsonFormat0(() => DirectionNhTransform)
  private implicit val cardinalityNhTransform = jsonFormat0(() => CardinalityNhTransform)
  private implicit val propertyNhTransformFormat = jsonFormat1(PropertyNhTransform.apply)
  private implicit val suffixNeighborhoodTransformFormat = jsonFormat1(SuffixNhTransform.apply)
  private implicit val keywordNeighborhoodTransformFormat = jsonFormat1(KeywordNhTransform.apply)
  private implicit val verbnetTransformFormat = jsonFormat1(VerbnetTransform.apply)

  implicit val neighborhoodTransformJsonFormat = parentFormat[NeighborhoodTransform](
    childFormat[ArclabelNhTransform.type, NeighborhoodTransform],
    childFormat[DirectionNhTransform.type, NeighborhoodTransform],
    childFormat[CardinalityNhTransform.type, NeighborhoodTransform],
    childFormat[PropertyNhTransform, NeighborhoodTransform],
    childFormat[SuffixNhTransform, NeighborhoodTransform],
    childFormat[KeywordNhTransform, NeighborhoodTransform],
    childFormat[VerbnetTransform, NeighborhoodTransform]
  )
}

/** Maps the tokens of a neighborhood to a particular property in their token's property map.
  *
  * @param propertyName name of the desired property
  */
case class PropertyNhTransform(propertyName: Symbol) extends NeighborhoodTransform {
  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    Seq(
      FeatureName(event.tokens map { tok =>
        parse.tokens(tok).getDeterministicProperty(propertyName)
      })
    )
  }
}

/** Creates a feature for every suffix (from a dictionary of suffixes) that appears in
  * the input neighborhood.
  *
  * @param keysuffixes the set of suffixes to consider
  */
case class SuffixNhTransform(keysuffixes: Seq[String])
    extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    event.tokens flatMap { tok =>
      keysuffixes filter { suffix =>
        parse.tokens(tok).word.name.toLowerCase.endsWith(suffix.toLowerCase)
      } map { suffix =>
        FeatureName(Seq(Symbol(suffix)))
      }
    }
  }
}

/** Creates a feature for every keyword (from a dictionary of keywords) that appears in
  * the input neighborhood.
  *
  * Note that the keyword matching is case-insensitive.
  *
  * @param keywords the set of words to consider
  */
case class KeywordNhTransform(keywords: Seq[String])
    extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    event.tokens flatMap { tok =>
      keywords filter { keyword =>
        parse.tokens(tok).word.name.toLowerCase == keyword.toLowerCase
      } map { suffix =>
        FeatureName(Seq(Symbol(suffix.toLowerCase)))
      }
    }
  }
}

/** Creates a feature per frame for all verbnet frames corresponding to the tokens
  * in the input neighborhood.
  *
  * @param keywords the set of words to consider
  */
case class VerbnetTransform(verbnet: Verbnet)
    extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    for {
      tokIx <- event.tokens
      featureName <- verbnet.getVerbnetFramePrimaryNames(MorphaStemmer.lemmatize(
        parse.tokens(tokIx).word.toString,
        parse.tokens(tokIx).getDeterministicProperty('factoriePos).toString
      ))
    } yield {
      FeatureName(Seq(featureName))
    }
  }
}

/** Creates a feature for the label on the arc connecting two tokens in a two-token neighborhood.
  *
  * Note that the apply operator will throw an exception if the argument neighborhood does
  * not have exactly two tokens. It will also throw an exception if the parse does not
  * contains an arc between the two neighborhood nodes.
  */
case object ArclabelNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    require(
      event.tokens.size == 2,
      s"cannot call ArclabelNhTransform on a neighborhood with ${event.tokens.size} tokens"
    )
    require(
      parse.arcLabelByEndNodes.contains(Set(event.tokens(0), event.tokens(1))),
      s"there is no arc between token ${event.tokens(0)} and ${event.tokens(1)}"
    )
    Seq(
      FeatureName(Seq(
        parse.arcLabelByEndNodes(Set(event.tokens(0), event.tokens(1)))
      ))
    )
  }
}

/** Creates a feature describing the order of two tokens in a two-token neighborhood.
  *
  * Specifically, it will be 'L if the first token appears to the left of the second token in
  * the sentence. Otherwise, it will be 'R.
  *
  * Note that the apply operator will throw an exception if the argument neighborhood does
  * not have exactly two tokens. It will also throw an exception if the two tokens are
  * the same (i.e. neither appears to the left of the other).
  */
case object DirectionNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    require(
      event.tokens.size == 2,
      s"cannot call ArclabelNhTransform on a neighborhood with ${event.tokens.size} tokens"
    )
    require(
      event.tokens(0) != event.tokens(1),
      s"cannot call ArclabelNhTransform on a neighborhood with a duplicate token"
    )
    val direction = (event.tokens(0) < event.tokens(1)) match {
      case true => 'L
      case false => 'R
    }
    Seq(
      FeatureName(Seq(direction))
    )
  }
}

/** Creates a feature describing the cardinality of a neighborhood, i.e. the number of tokens
  * in the neighborhood.
  */
case object CardinalityNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    Seq(FeatureName(Seq(Symbol(event.tokens.size.toString))))
  }
}
