package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.fsm.NbestList

import scala.io.Source
import scala.util.Random

import reming.{ CompactPrinter, JsonParser }
import reming.DefaultJsonProtocol._

/** A ParsePool is a collection of parse candidates for the same input sentence.
  *
  * @param parses a sequence of parse trees
  */
case class ParsePool(parses: Iterable[(PolytreeParse, Double)]) {
  def toNbestList: NbestList = {
    NbestList(parses)
  }

  @transient lazy val indexedParses = parses.toIndexedSeq

  def chooseRandomParse: PolytreeParse = {
    indexedParses(Random.nextInt(indexedParses.size))._1
  }
}

object ParsePool {
  implicit val jsFormat = jsonFormat1(ParsePool.apply)
}

/** A data source for ParsePool objects. */
trait ParsePoolSource {
  def poolIterator: Iterator[ParsePool]
}

case class InMemoryParsePoolSource(inputIterator: Iterator[ParsePool]) extends ParsePoolSource {

  private val cachedPools = inputIterator.toIterable

  override def poolIterator: Iterator[ParsePool] = {
    cachedPools.iterator
  }
}

case class FileBasedParsePoolSource(filename: String) extends ParsePoolSource {

  override def poolIterator: Iterator[ParsePool] = {
    val lines: Iterator[String] = Source.fromFile(filename).getLines
    lines map { line =>
      JsonParser.read[ParsePool](line)
    }
  }
}

object FileBasedParsePoolSource {

  def writePools(pools: Iterator[ParsePool], filename: String) {
    Resource.using(new PrintWriter(new File(filename))) { writer =>
      for (pool <- pools) {
        CompactPrinter.printTo(writer, pool)
      }
    }
  }
}

