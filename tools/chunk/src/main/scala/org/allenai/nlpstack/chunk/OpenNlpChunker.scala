package org.allenai.nlpstack.chunk

import org.allenai.common.Resource
import org.allenai.nlpstack.core.{ ChunkedToken, Chunker, PostaggedToken }

import opennlp.tools.chunker.{ ChunkerME, ChunkerModel }

import java.net.URL

class OpenNlpChunker(val model: ChunkerModel) extends Chunker {
  def this() = this(OpenNlpChunker.loadDefaultModel())

  val chunker = new ChunkerME(model)

  def chunkPostagged(tokens: Seq[PostaggedToken]): Seq[ChunkedToken] = {
    val chunks = chunker.chunk(tokens.map(_.string).toArray, tokens.map(_.postag).toArray)
    (tokens zip chunks) map { case (token, chunk) => ChunkedToken(token, chunk) }
  }
}

object OpenNlpChunker {
  private def defaultModelName = "en-chunker.bin"
  val defaultModelUrl: URL = {
    val url = this.getClass.getClassLoader.getResource(defaultModelName)
    require(url != null, "Could not load default chunker model: " + defaultModelName)
    url
  }

  def loadDefaultModel(): ChunkerModel = loadModel(defaultModelUrl)

  private def loadModel(url: URL): ChunkerModel = {
    Resource.using(url.openStream()) { stream =>
      new ChunkerModel(stream)
    }
  }
}
