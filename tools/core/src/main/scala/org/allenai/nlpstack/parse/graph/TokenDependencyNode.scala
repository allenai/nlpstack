package org.allenai.nlpstack.parse.graph

import org.allenai.common.immutable.Interval
import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.graph.Graph._
import org.allenai.nlpstack.lemmatize._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._

import scala.collection.immutable.SortedSet

/** A representation for a node in the graph of dependencies.  A node
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
