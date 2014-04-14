package org.allenai
package aitk
package parse
package graph

import org.allenai.aitk.graph.Graph
import org.allenai.aitk.graph.Graph._
import scala.collection.immutable.SortedSet
import org.allenai.common.immutable.Interval
import aitk.lemmatize.{ Stemmer, IdentityStemmer }
import aitk.postag.PostaggedToken
import org.allenai.aitk.tokenize.Token
import org.allenai.aitk.lemmatize.Lemmatized

/**
  * A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
case class TokenDependencyNode(val id: Int, val token: Lemmatized[PostaggedToken]) {
  def string = token.token.string
  def postag = token.token.postag
  def lemma = token.lemma

  // extend Object
  override def toString() = s"$string-$id"
}

object TokenDependencyNode {
  def from(tokens: Seq[Lemmatized[PostaggedToken]])(node: DependencyNode) =
    TokenDependencyNode(node.id, tokens(node.id))
}
