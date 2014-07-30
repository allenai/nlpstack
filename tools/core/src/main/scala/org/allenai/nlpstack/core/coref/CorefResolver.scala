package org.allenai.nlpstack.core.coref

import org.allenai.nlpstack.core.Format
import org.allenai.nlpstack.core.Token
import org.allenai.nlpstack.core.parse.graph.{ DependencyNode, DependencyGraph }

case class Referent(
  val references: Seq[DependencyNode],
  val mainReference: Option[DependencyNode])

abstract class CorefResolver[T <: Token] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)): Seq[Referent]
}

object CorefResolver {
  object multilineStringFormat extends StringFormat("\n")

  class StringFormat(val delimiter: String) extends Format[Seq[Referent], String] {
    override def read(from: String): Seq[Referent] = sys.error("foo")

    override def write(from: Seq[Referent]): String = from.map(r =>
      "(%s) refer to %s".format(r.references.mkString(", "), r.mainReference)).mkString(delimiter)
  }
}
