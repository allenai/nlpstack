package org.allenai.nlpstack.parse.poly.polyparser

import java.nio.file.Paths

import org.allenai.datastore.Datastore
import org.allenai.nlpstack.parse.poly.core.{ Sentence, SentenceSource }

/** A data source for PolytreeParse objects. */
trait PolytreeParseSource extends SentenceSource {
  def parseIterator: Iterator[PolytreeParse]
}

case class InMemoryPolytreeParseSource(
    parses: Iterable[PolytreeParse]
) extends PolytreeParseSource {

  override def parseIterator: Iterator[PolytreeParse] = {
    parses.iterator
  }

  override def sentenceIterator: Iterator[Sentence] = {
    parseIterator map { _.sentence }
  }
}

object InMemoryPolytreeParseSource {

  /** Create an InMemoryPolytreeParseSource from a filename and a file format.
    *
    * If the dataSource argument is specified as "datastore", it will look for the
    * file in the AI2 private datastore. Otherwise it will look for the file on the local drive.
    *
    * @param filename the name of the file containing the parses
    * @param fileFormat the file format of the parse file
    * @param dataSource where to look for the file ("datastore" for the AI2 datastore,
    * "local" for the local drive)
    * @return the constructed parse source
    */
  def getParseSource(filename: String, fileFormat: PolytreeParseFileFormat,
    dataSource: String = "local"): PolytreeParseSource = {

    val parseFilename: String = dataSource match {
      case "datastore" =>
        val path: java.nio.file.Path =
          Datastore("private").directoryPath(
            "org.allenai.corpora.parsing",
            "treebanks",
            1
          )
        Paths.get(path.toString, filename).toString
      case _ =>
        filename
    }
    InMemoryPolytreeParseSource(PolytreeParse.fromFile(parseFilename, fileFormat).toIterable)
  }
}

/** Creates a data source from a file of parse trees.
  *
  * @param filename the file containing the parse trees
  * @param format the file format
  */
case class FileBasedPolytreeParseSource(
    filename: String,
    format: PolytreeParseFileFormat
) extends PolytreeParseSource {

  override def parseIterator: Iterator[PolytreeParse] = {
    PolytreeParse.fromFile(filename, format)
  }

  override def sentenceIterator: Iterator[Sentence] = {
    PolytreeParse.fromFile(filename, format) map { _.sentence }
  }
}

object FileBasedPolytreeParseSource {

  /** Create a FileBasedPolytreeParseSource from a filename and a file format.
    *
    * If the dataSource argument is specified as "datastore", it will look for the
    * file in the AI2 private datastore. Otherwise it will look for the file on the local drive.
    *
    * @param filename the name of the file containing the parses
    * @param fileFormat the file format of the parse file
    * @param dataSource where to look for the file ("datastore" for the AI2 datastore,
    * "local" for the local drive)
    * @return the constructed parse source
    */
  def getParseSource(filename: String, fileFormat: PolytreeParseFileFormat,
    dataSource: String = "local"): PolytreeParseSource = {

    val parseFilename: String = dataSource match {
      case "datastore" =>
        val path: java.nio.file.Path =
          Datastore("private").directoryPath(
            "org.allenai.corpora.parsing",
            "treebanks",
            1
          )
        Paths.get(path.toString, filename).toString
      case _ =>
        filename
    }
    FileBasedPolytreeParseSource(parseFilename, fileFormat)
  }
}

case class MultiPolytreeParseSource(parseSources: Iterable[PolytreeParseSource])
    extends PolytreeParseSource {

  override def parseIterator: Iterator[PolytreeParse] = {
    (parseSources map { _.parseIterator }) reduce { (x, y) => x ++ y }
  }

  override def sentenceIterator: Iterator[Sentence] = {
    (parseSources map { _.sentenceIterator }) reduce { (x, y) => x ++ y }
  }
}

sealed abstract class PolytreeParseFileFormat
case class ConllX(useGoldPOSTags: Boolean, makePoly: Boolean = false)
  extends PolytreeParseFileFormat
