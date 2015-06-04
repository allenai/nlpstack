package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.Resource
import org.allenai.nlpstack.core.{
  Token => NlpStackToken,
  PostaggedToken,
  Postagger,
  Tokenizer
}
import reming.{ JsonFormat, JsonParser }

import java.io.{ File, InputStream, PushbackInputStream }
import java.net.URL
import java.util.zip.GZIPInputStream

import scala.io.BufferedSource

object Util {
  def readFromFile[T: JsonFormat](filename: String): T = {
    readFromUrl(new File(filename).toURI.toURL)
  }

  def readFromUrl[T: JsonFormat](url: URL): T = {
    Resource.using(url.openStream()) { readFromStream[T] }
  }

  def readFromStream[T: JsonFormat](stream: InputStream): T = {
    val headerLength = 2
    val pbStream = new PushbackInputStream(stream, headerLength)
    val header = new Array[Byte](headerLength)
    val readBytes = pbStream.read(header, 0, headerLength)
    pbStream.unread(header, 0, readBytes)

    val isZipped =
      (readBytes == headerLength) &&
        (header(0) == GZIPInputStream.GZIP_MAGIC.toByte) &&
        (header(1) == (GZIPInputStream.GZIP_MAGIC >> 8).toByte)

    val uncompressedStream =
      if (isZipped) {
        new GZIPInputStream(pbStream)
      } else {
        pbStream
      }

    JsonParser.read[T](new BufferedSource(uncompressedStream))
  }

  /** Uses an NlpStack postagger to tag a Sentence object.
    *
    * @param sentence the Sentence to tag
    * @param posTagger the nlpstack postagger to use
    * @return a map from Sentence token indices to their POS tags
    */
  def getPostaggedTokens(sentence: Sentence, posTagger: Postagger): Map[Int, PostaggedToken] = {
    val words: IndexedSeq[String] = sentence.tokens.tail map { tok => tok.word.name }
    val nlpStackTokens: IndexedSeq[NlpStackToken] =
      Tokenizer.computeOffsets(words, words.mkString).toIndexedSeq
    (posTagger.postagTokenized(nlpStackTokens).zipWithIndex map {
      case (taggedTok, index) =>
        (index + 1, taggedTok)
    }).toMap
  }
}
