package org.allenai
package aitk
package parse
package graph

import org.allenai.aitk.graph.Graph
import org.allenai.aitk.graph.Graph._
import org.allenai.common.immutable.Interval
import aitk.lemmatize.{ Stemmer, IdentityStemmer }
import aitk.postag.PostaggedToken

import scala.collection.immutable.SortedSet
import scala.util.matching.Regex

/**
  * A representation for a node in the graph of dependencies.  A node
  * represents one or more adjacent tokens in the source sentence.
  */
case class DependencyNode(val id: Int, val string: String) {
  require(string != null)

  // extend Object
  override def toString() = s"$string-$id"
}

object DependencyNode {
  implicit object DependencyNodeOrdering extends Ordering[DependencyNode] {
    def compare(a: DependencyNode, b: DependencyNode) = a.id compare b.id
  }

  object stringFormat extends Format[DependencyNode, String] {
    val Serialized = new Regex("""(\p{Graph}*?)-(\d\d*)""")
    def write(node: DependencyNode): String = {
      val cleanText = node.string.replaceAll("[[_()][^\\p{Graph}]]", "")
      Iterator(cleanText, node.id).mkString("-")
    }

    def read(pickled: String): DependencyNode = {
      val (text, id) = pickled match {
        case Serialized(text, id) => (text, id)
        case _ => throw new MatchError("Could not split pickled node into parts: " + pickled)
      }

      new DependencyNode(id.toInt, text)
    }
  }

  @deprecated("Use StringFormat instead.", "2.4.5")
  def deserialize(string: String) = {
    stringFormat.read(string)
  }

  class SerializationException(message: String, cause: Throwable)
    extends RuntimeException(message, cause)
}
