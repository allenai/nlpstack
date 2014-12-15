package org.allenai.nlpstack.parse.poly.core

import java.io.{ File, InputStream, PushbackInputStream }
import java.net.URL
import java.util.zip.GZIPInputStream

import org.allenai.common.Resource._
import spray.json._

import scala.io.Source

object Util {

  def getJsValueFromFile(filename: String): JsValue = {
    getJsValueFromUrl(new File(filename).toURI.toURL)
  }

  def getJsValueFromUrl(url: URL): JsValue = {
    using(url.openStream()) { getJsValueFromStream }
  }

  def getJsValueFromStream(stream: InputStream): JsValue = {
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

    Source.fromInputStream(uncompressedStream).mkString.parseJson
  }
}
