package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.core.Tokenizer
import java.io.InputStream
import org.allenai.nlpstack.parse.poly.core.Sentence

import scala.io.Source

/** A function that adds new token properties to a sentence if that token appears
  * within a multi-word expression in the dictionary. The new properties are
  *
  * MultiWordTagger.mweSymbol -> MultiWordTagger.mweValue
  *
  * and
  *
  * MultiWordTagger.symbolFor(mwe) -> MultiWordTagger.mweValue
  *
  * The first property encodes the the fact that the token appears within any
  * MWE. The second property encodes the fact that the token appears within
  * a particular MWE. Tokens that do not occur within a particular MWE will not
  * be given any additional properties.
  */
/*
case class MultiWordTagger(dictionary: Set[IndexedSeq[Symbol]]) extends (Sentence => Sentence) {

  import MultiWordTagger.{ mweSymbol, mweValue, symbolFor }

  /** Identifies MWEs in the input sentence. Returns a copy of the input sentence with additional
    * MWE token properties.
    * @param sentence the original sentence
    * @return a new sentence with additional MWE token properties
    */
  def apply(sentence: Sentence): Sentence = addProperties(sentence)

  private def addProperties(sentence: Sentence): Sentence = {

    // Get a map from token index to the set of new properties to add to that token
    val indexProperties: Map[Int, Seq[(Symbol, String)]] =
      groupByTuple2(properties(sentence)).mapValues(_.flatten)

    // Get the combined old + new properties properties for that token
    val newTokens = for {
      (token, i) <- sentence.tokens.zipWithIndex
      newProperties = indexProperties.getOrElse(i, Seq[(Symbol, String)]())
      oldProperties = token.properties
      newToken = token.copy(properties = oldProperties ++ newProperties)
    } yield newToken

    // Return the new sentence, same tokens but with updated properties
    sentence.copy(tokens = newTokens)

  }

  private val maxLength: Int = if (dictionary.isEmpty) 0 else dictionary.map(_.size).max

  // TODO(tonyf): replace brute-force search with a trie or something
  private def properties(sentence: Sentence): Seq[(Int, Seq[(Symbol, String)])] = for {

    // Get all spans of the input sentence
    (start, end, tokens) <- spans(sentence)

    // If the span is a MWE
    if isMwe(tokens)

    // Construct new properties to add to the tokens in this span
    tokenProperties = Seq((mweSymbol, mweValue), (symbolFor(tokens), mweValue))

    // For each token in the span, yield its new properties
    k <- start until end

  } yield (k, tokenProperties)

  private def isMwe(tokens: IndexedSeq[Symbol]): Boolean = dictionary contains tokens

  private def spans(sentence: Sentence): Seq[(Int, Int, IndexedSeq[Symbol])] = for {
    start <- 0 until sentence.size
    length <- 1 to maxLength
    end = start + length
    tokens = sentence.tokens.slice(start, end).toIndexedSeq.map(_.word)
  } yield (start, end, tokens)

  private def groupByTuple2[K, V](seq: Seq[(K, V)]): Map[K, Seq[V]] =
    seq.groupBy(_._1).mapValues(_.map(_._2))

}
*/

case object MultiWordTagger {

  /** The symbol used to represent the property "token appears within some MWE".
    */
  val mweSymbol = 'mwe

  /** The symbol used to represent the property "token appears with this particular
    * MWE".
    */
  def symbolFor(mwe: Seq[Symbol]): Symbol = {
    val string = mwe.map(_.name).mkString(" ")
    Symbol(s"${mweSymbol.name}(${string})")
  }

  /** A predicate that tests whether a property key is a MWE property.
    * @param key the input key
    * @return true if `key` is a MWE property key, false otherwise
    */
  def mweProperty(key: Symbol): Boolean = key.name.startsWith("${mweSymbol.name}(")

  /** The token properties are boolean indicators, so the MWE token properties will
    * have this placeholder value.
    */
  val mweValue = "true"

  /** Reads the given MultiWordTagger dictionary from the input stream. Assumes
    * one entry per line.
    * param input the input stream
    * param tokenizer a tokenizer object used to tokenize the MWE dictionary entries
    * @return a new `MultiWordTagger` object
    */
  /*
  def fromStream(input: InputStream)(implicit tokenizer: Tokenizer): MultiWordTagger = {
    val lines = Source.fromInputStream(input).getLines
    val tokenized = lines.map(line => tokenizer.tokenize(line).toIndexedSeq)
    val dictionary = tokenized.map(seq => seq.map(token => Symbol(token.string)))
    MultiWordTagger(dictionary.toSet)
  }
  */

}
