package org.allenai.nlpstack.chunk

import org.allenai.common.Resource
import org.allenai.nlpstack.core.{ ChunkedToken, Chunker, PostaggedToken }

import opennlp.tools.chunker.{ ChunkerME, ChunkerModel }

class OpenNlpChunker extends Chunker {
  private val chunker = new ChunkerME(OpenNlpChunker.model)
  private final val chunker_t: ThreadLocal[ChunkerME] = new ThreadLocal[ChunkerME]() {
    override protected def initialValue(): ChunkerME = new ChunkerME(OpenNlpChunker.model)
  }

  def chunkPostaggedOriginal(tokens: Seq[PostaggedToken]): Seq[ChunkedToken] = {
    // OpenNLP uses : as the postag for hyphens, but we use HYPH, so we change it back before
    // sending it to the chunker.
    val fixedTokens = tokens.map { t =>
      if (t.string == "-") PostaggedToken(t, ":") else t
    }

    val chunks = chunker_t.get().chunk(tokens.map(_.string).toArray, fixedTokens.map(_.postag)
      .toArray)
    (tokens zip chunks) map { case (token, chunk) => ChunkedToken(token, chunk) }
  }

  //Open NLP runs into random NullPointerExceptions. The following is a workaround that reduces the
  // probability of running into errors
//  def chunkPostagged(tokens: Seq[PostaggedToken]): Seq[ChunkedToken] = {
//    def helper(tries: Int): Seq[ChunkedToken] = try {
//      if (tries == 0) throw new java.lang.NullPointerException()
//      else chunkPostaggedOriginal(tokens)
//    } catch {
//      case e: Throwable => helper(tries - 1)
//    }
//    helper(0)
//  }
  def chunkPostagged(tokens: Seq[PostaggedToken]): Seq[ChunkedToken] = chunkPostaggedOriginal(tokens)
}

object OpenNlpChunker {
  private val defaultModelName = "en-chunker.bin"
  private val model = Resource.using(this.getClass.getClassLoader.getResourceAsStream(defaultModelName)) { is =>
    new ChunkerModel(is)
  }
}
