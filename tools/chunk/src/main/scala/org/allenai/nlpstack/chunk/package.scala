package org.allenai.nlpstack

import org.allenai.nlpstack.core.Chunker

package object chunk {
  val defaultChunker: Chunker = new OpenNlpChunker
}