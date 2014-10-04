package org.allenai.nlpstack.chunk

import org.allenai.common.Resource
import org.allenai.nlpstack.core.{ ChunkedToken, Chunker, PostaggedToken }

import opennlp.tools.chunker.{ ChunkerME, ChunkerModel }

class OpenNlpChunker extends Chunker {
  private val chunker = new ChunkerME(OpenNlpChunker.model)

  def chunkPostagged(tokens: Seq[PostaggedToken]): Seq[ChunkedToken] = {
    // OpenNLP uses : as the postag for hyphens, but we use HYPH, so we change it back before
    // sending it to the chunker.
    val fixedTokens = tokens.map { t =>
      if (t.string == "-") PostaggedToken(t, ":") else t
    }

    val chunks = chunker.chunk(tokens.map(_.string).toArray, fixedTokens.map(_.postag).toArray)
    (tokens zip chunks) map { case (token, chunk) => ChunkedToken(token, chunk) }
  }
}

object OpenNlpChunker {
  private val defaultModelName = "en-chunker.bin"
  private val model = Resource.using(this.getClass.getClassLoader.getResourceAsStream(defaultModelName)) { is =>
    new ChunkerModel(is)
  }
}
