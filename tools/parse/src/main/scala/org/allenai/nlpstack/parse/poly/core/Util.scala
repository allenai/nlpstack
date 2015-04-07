package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.Resource
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
}
