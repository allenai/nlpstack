package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{File, PrintWriter}

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.fsm.NbestList

import scala.io.Source
import spray.json._
import spray.json.DefaultJsonProtocol._

/** A ParsePool is a collection of parse candidates for the same input sentence.
  *
  * @param parses a sequence of parse trees
  */
case class ParsePool(parses: Iterable[(PolytreeParse, Double)]) {
  def toNbestList: NbestList = {
    NbestList(parses)
  }
}

object ParsePool {
  implicit val jsFormat = jsonFormat1(ParsePool.apply)
}

/** A data source for ParsePool objects. */
trait ParsePoolSource {
  def poolIterator: Iterator[ParsePool]
}

case class FileBasedParsePoolSource(filename: String) extends ParsePoolSource {

  override def poolIterator: Iterator[ParsePool] = {
    val lines: Iterator[String] = Source.fromFile(filename).getLines
    lines map { line =>
      line.parseJson.convertTo[ParsePool]
    }
  }
}

object FileBasedParsePoolSource {

  def writePools(pools: Iterator[ParsePool], filename: String) {
    Resource.using(new PrintWriter(new File(filename))) { writer =>
      for (pool <- pools) {
        writer.println(pool.toJson.compactPrint)
      }
    }
  }
}

