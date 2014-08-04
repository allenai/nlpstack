package org.allenai.nlpstack.core.coref

import org.allenai.nlpstack.core.Format.Quoter
import org.allenai.nlpstack.core.{ PostaggedToken, Format, Token }
import org.allenai.nlpstack.core.parse.graph.{ DependencyNode, DependencyGraph }

import scala.util.matching.Regex
import java.util.regex.Pattern

case class Referent(
  val references: Seq[DependencyNode],
  val mainReference: Option[DependencyNode])

abstract class CorefResolver[T <: Token] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)): Seq[Referent]
}

object CorefResolver {
  object multilineStringFormat extends StringFormat("\n")
  object singlelineStringFormat extends StringFormat(";")

  class StringFormat(val separator: String) extends Format[(DependencyGraph, Seq[Referent]), String] {
    private val dgraphStringFormat = new DependencyGraph.StringFormat(separator)

    private val regex =
      new Regex("""^\((.*[^)])\)( refers to (.*))?$""", "list", "_", "mainRef")

    override def read(from: String): (DependencyGraph, Seq[Referent]) = {
      val parts = from.split(Pattern.quote(separator * 2), 2)
      require(parts.length == 2)
      val (dgraphString, corefString) = (parts(0), parts(1))

      val dgraph = dgraphStringFormat.read(dgraphString)

      val coref = corefString.split(Pattern.quote(separator)).map(s => {
        val m = regex.findFirstMatchIn(s)
        require(m.isDefined)
        val stringReferences = m.get.group("list").split(Pattern.quote(", "))
        val references = stringReferences map DependencyNode.stringFormat.read

        val mainReference = m.get.group("mainRef") match {
          case null => None
          case mainRefString => Some(DependencyNode.stringFormat.read(mainRefString))
        }

        Referent(references, mainReference)
      })

      (dgraph, coref)
    }

    override def write(from: (DependencyGraph, Seq[Referent])): String = {
      val (dgraph, coref) = from
      dgraphStringFormat.write(dgraph) +
        separator +
        separator +
        coref.map(r =>
          "(%s)".format(r.references.mkString(", ")) + (r.mainReference match {
            case None => ""
            case Some(node) => " refers to %s".format(node)
          })).mkString(separator)
    }
  }
}
