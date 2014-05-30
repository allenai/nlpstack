package org.allenai
package nlpstack
package parse
package graph

import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.graph.Graph._
import scala.collection.immutable.SortedSet
import org.allenai.common.immutable.Interval
import nlpstack.lemmatize.{ Stemmer, IdentityStemmer }
import nlpstack.postag.PostaggedToken
import org.allenai.nlpstack.tokenize.Token
import org.allenai.nlpstack.lemmatize.Lemmatized

/**
  * A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
case class TokenDependencyNode(val id: Int, val lemmatizedToken: Lemmatized[PostaggedToken]) {
  def string = token.string
  def postag = token.postag
  def lemma = lemmatizedToken.lemma

  def token: PostaggedToken = lemmatizedToken.token

  // extend Object
  override def toString() = s"$string-$id"
}

object TokenDependencyNode {
  def from(tokens: Seq[Lemmatized[PostaggedToken]])(node: DependencyNode) =
    TokenDependencyNode(node.id, tokens(node.id))
}
