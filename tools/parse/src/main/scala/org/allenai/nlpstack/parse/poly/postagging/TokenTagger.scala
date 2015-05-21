package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.Token

case class TokenTag(name: Symbol, value: Symbol)

trait TokenTagger {
  def tag(token: Token): Set[TokenTag]
}

case class KeywordTagger(keywords: Set[String]) extends TokenTagger {

  def tag(token: Token): Set[TokenTag] = {
    val tokStr = token.word.name.toLowerCase
    if (keywords.contains(tokStr)) {
      Set(TokenTag('keyword, Symbol(tokStr)))
    } else {
      Set()
    }
  }
}

case object LexicalPropertiesTagger extends TokenTagger {

  def tag(token: Token): Set[TokenTag] = {
    val tokStr = token.word.name
    val firstLetterCapital = tokStr.headOption match {
      case Some(x) if Character.isUpperCase(x) => Some('firstCap)
      case _ => None
    }
    val existsCapital = tokStr match {
      case tokStr: String if tokStr exists {
        Character.isUpperCase
      } => Some('existsCap)
      case _ => None
    }
    val allCaps = tokStr match {
      case tokStr: String if tokStr forall {
        Character.isUpperCase
      } => Some('allCaps)
      case _ => None
    }
    val existsNumber = tokStr match {
      case tokStr: String if tokStr exists {
        Character.isDigit
      } => Some('existsNum)
      case _ => None
    }
    (Seq(firstLetterCapital, existsCapital, allCaps, existsNumber) map { maybeFeat =>
      maybeFeat map { feat =>
        TokenTag('lexical, feat)
      }
    }).flatten.toSet
  }
}

