package org.allenai.nlpstack.core

import org.allenai.common.testkit.UnitSpec

import spray.json._

class TokenSpec extends UnitSpec {
  "tokens" should "round trip through string serialization" in {
    val t = Token("asdf", 0)
    val pickled = Token.stringFormat.write(t)
    val unpickled = Token.stringFormat.read(pickled)
    assert(unpickled === t)
  }

  it should "round trip through string serialization when it contains an @" in {
    val t = Token("@", 0)
    assert(Token.stringFormat.read(Token.stringFormat.write(t)) === t)
  }

  it should "throw an exception when pickled string has a non-integer offset" in {
    val tokenSerializationString = "The@nonInteger"
    intercept[MatchError] {
      Token.stringFormat.read(tokenSerializationString)
    }
  }

  it should "round trip through json serialization" in {
    val t = Token("asdf", 0)
    val pickled = Token.tokenJsonFormat.write(t)
    val unpickled = Token.tokenJsonFormat.read(pickled)
    assert(unpickled === t)
  }

  "pos-tagged tokens" should "round trip through string serialization" in {
    val pt = PostaggedToken("DT", "in", 3)
    assert(PostaggedToken.stringFormat.read(PostaggedToken.stringFormat.write(pt)) === pt)
  }

  it should "round trip through json serialization" in {
    val t = PostaggedToken("DT", "in", 3)
    val pickled = PostaggedToken.postaggedTokenJsonFormat.write(t)
    val unpickled = PostaggedToken.postaggedTokenJsonFormat.read(pickled)
    assert(unpickled === t)
  }

  "chunked tokens" should "round trip through json serialization" in {
    val t = ChunkedToken("string", "DT", "B-NP", 0)
    val pickled = ChunkedToken.chunkedTokenJsonFormat.write(t)
    val unpickled = ChunkedToken.chunkedTokenJsonFormat.read(pickled)
    assert(unpickled === t)
  }

  it should "deserialize from json ok" in {
    val t = ChunkedToken("B-NP", "DT", "asdf", 4)
    val json = """{ "string" : "asdf", "postag" : "DT", "chunk" : "B-NP", "offset" : 4}"""

    assert(ChunkedToken.chunkedTokenJsonFormat.read(json.parseJson) === t)
  }

  "tokenizer serialization" should "round trip" in {
    val token1 = Token("The", 0)
    val token2 = Token("big", 4)
    val tokens = Seq(token1, token2)
    val tokensSerialization = Tokenizer.stringFormat.write(tokens)
    assert(Tokenizer.stringFormat.read(tokensSerialization) === tokens)
  }

  "pos-tagger serialization" should "rount trip" in {
    val posToken1 = PostaggedToken("DT", "The", 0)
    val posToken2 = PostaggedToken("JJ", "big", 4)
    val posTokens = Seq(posToken1, posToken2)
    val posTokensSerialization = Postagger.stringFormat.write(posTokens)
    assert(Postagger.stringFormat.read(posTokensSerialization) === posTokens)
  }

  "chunker serialization" should "round trip" in {
    val chunkedToken1 = ChunkedToken("NP-DT", "DT", "The", 0)
    val chunkedToken2 = ChunkedToken("NP-JJ", "JJ", "big", 4)
    val chunkedTokens = Seq(chunkedToken1, chunkedToken2)
    val serializedChunkedTokens = Chunker.stringFormat.write(chunkedTokens)
    assert(Chunker.stringFormat.read(serializedChunkedTokens) === chunkedTokens)
  }
}
