package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.{ FactorieUtilities, Format, Tokenizer, Token }
import org.allenai.nlpstack.tokenize.FactorieTokenizer.factorieFormat

import cc.factorie.app.nlp.{ Document => FactorieDocument, Token => FactorieToken, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap }
import cc.factorie.app.nlp.segment.DeterministicTokenizer

class FactorieTokenizer extends Tokenizer {
  private val tokenizer =
    new DeterministicTokenizer(tokenizeAllDashedWords = false)
  private val map = new MutableDocumentAnnotatorMap ++=
    DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
  map += tokenizer
  private val pipeline = DocumentAnnotatorPipeline(
    map = map.toMap,
    prereqs = Nil,
    tokenizer.postAttrs)

  def tokenize(sentence: String): Seq[Token] = {
    val doc = pipeline.process(
      new FactorieDocument(
        FactorieUtilities.replaceUnclosedTag(sentence)))

    factorieFormat.read(doc)
  }
}

object FactorieTokenizer {
  object factorieFormat extends Format[Seq[Token], FactorieDocument] {
    override def read(from: FactorieDocument): Seq[Token] = {
      val result = for (section <- from.sections; token <- section.tokens)
        yield Token(token.string, token.stringStart)

      // glue hyphenated words back together
      def glueHyphenated(tokens: Seq[Token]): Seq[Token] = {
        // special case bonanza
        if (tokens.length < 2) {
          tokens
        } else {
          val i = tokens.indexWhere(_.string == "-")
          if (i >= 0) {
            val hyphenToken = tokens(i)
            i match {
              case 0 =>
                val nextToken = tokens(i + 1)
                if (nextToken.offset == hyphenToken.offset + 1) {
                  glueHyphenated(
                    Token("-" + tokens(i + 1).string, hyphenToken.offset) +: tokens.drop(2))
                } else {
                  hyphenToken +: glueHyphenated(tokens.drop(1))
                }

              case x if x == tokens.length - 1 =>
                val prevToken = tokens(i - 1)
                if (prevToken.offset + prevToken.string.length == hyphenToken.offset) {
                  tokens.dropRight(2) :+ Token(prevToken.string + "-", prevToken.offset)
                } else {
                  tokens
                }

              case _ =>
                val prevToken = tokens(i - 1)
                val nextToken = tokens(i + 1)
                val prevTokenMatches =
                  prevToken.offset + prevToken.string.length == hyphenToken.offset
                val nextTokenMatches =
                  hyphenToken.offset + 1 == nextToken.offset
                if (prevTokenMatches && nextTokenMatches) {
                  val combinedToken =
                    Token(prevToken.string + "-" + nextToken.string, prevToken.offset)
                  tokens.take(i - 1) ++ glueHyphenated(combinedToken +: tokens.drop(i + 2))
                } else if (prevTokenMatches) {
                  val combinedToken =
                    Token(prevToken.string + "-", prevToken.offset)
                  (tokens.take(i - 1) :+ combinedToken) ++ glueHyphenated(tokens.drop(i + 1))
                } else if (nextTokenMatches) {
                  val combinedToken =
                    Token("-" + nextToken.string, hyphenToken.offset)
                  tokens.take(i) ++ glueHyphenated(combinedToken +: tokens.drop(i + 2))
                } else {
                  tokens.take(i + 1) ++ glueHyphenated(tokens.drop(i + 1))
                }
            }
          } else {
            tokens
          }
        }
      }

      glueHyphenated(result)
    }

    override def write(from: Seq[Token]): FactorieDocument = {
      val factorieDoc = new FactorieDocument(Tokenizer.originalText(from))
      for (token <- from) {
        // creating factorie tokens modifies the factorie document
        val factorieToken = new FactorieToken(
          factorieDoc,
          token.offset,
          token.offset + token.string.length)
        factorieToken.attr += token
      }
      factorieDoc
    }
  }
}
