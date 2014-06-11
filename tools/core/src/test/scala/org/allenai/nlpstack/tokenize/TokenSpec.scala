package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.chunk.ChunkedToken
import org.allenai.nlpstack.chunk.Chunker
import org.allenai.nlpstack.postag.PostaggedToken
import org.allenai.nlpstack.postag.Postagger
import org.allenai.common.testkit.UnitSpec

class TokenSpec extends UnitSpec {
  "tokens" should "round trip through serialization" in {
    val t = Token("asdf", 0)
    val pickled = Token.stringFormat.write(t)
    val unpickled = Token.stringFormat.read(pickled)
    unpickled === t
  }

  it should "round trip through serialization when it contains an @" in {
    val t = Token("@", 0)
    Token.stringFormat.read(Token.stringFormat.write(t)) == t
  }

  it should "throw an exception when pickled string has a non-integer offset" in {
    val tokenSerializationString = "The@nonInteger"
    intercept[MatchError] {
      Token.stringFormat.read(tokenSerializationString)
    }
  }

  "pos-tagged tokens" should "round trip through serialization" in {
    val pt = PostaggedToken("DT", "in", 3)
    PostaggedToken.stringFormat.read(PostaggedToken.stringFormat.write(pt)) === pt
  }

  "tokenizer serialization" should "round trip" in {
    val token1 = Token("The", 0)
    val token2 = Token("big", 4)
    val tokens = Seq(token1, token2)
    val tokensSerialization = Tokenizer.stringFormat.write(tokens)
    Tokenizer.stringFormat.read(tokensSerialization) === tokens
  }

  "pos-tagger serialization" should "rount trip" in {
    val posToken1 = PostaggedToken("DT", "The", 0)
    val posToken2 = PostaggedToken("JJ", "big", 4)
    val posTokens = Seq(posToken1, posToken2)
    val posTokensSerialization = Postagger.stringFormat.write(posTokens)
    Postagger.stringFormat.read(posTokensSerialization) === posTokens
  }

  "chunker serialization" should "round trip" in {
    val chunkedToken1 = ChunkedToken("NP-DT", "DT", "The", 0)
    val chunkedToken2 = ChunkedToken("NP-JJ", "JJ", "big", 4)
    val chunkedTokens = Seq(chunkedToken1, chunkedToken2)
    val serializedChunkedTokens = Chunker.stringFormat.write(chunkedTokens)
    Chunker.stringFormat.read(serializedChunkedTokens) === chunkedTokens
  }
}
