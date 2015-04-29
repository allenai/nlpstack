package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.{ Token, Tokenizer }

import scala.collection.mutable

object Ai2Tokenizer extends Tokenizer {
  var averageTokenLength = 6 // low estimates are better

  private object CharacterClass extends Enumeration {
    type CharacterClass = Value
    val Whitespace, Punctuation, Other = Value
  }
  import CharacterClass._
  private val characterClassMap = Map(
    ' ' -> Whitespace,
    '\n' -> Whitespace,
    '\r' -> Whitespace,
    '\t' -> Whitespace,
    ' ' -> Whitespace, // special kind of space?
    '.' -> Punctuation,
    ';' -> Punctuation,
    ':' -> Punctuation,
    '+' -> Punctuation,
    '{' -> Punctuation,
    '}' -> Punctuation,
    '[' -> Punctuation,
    ']' -> Punctuation,
    '(' -> Punctuation,
    ')' -> Punctuation,
    '*' -> Punctuation,
    '^' -> Punctuation,
    '%' -> Punctuation,
    '$' -> Punctuation,
    '#' -> Punctuation,
    '@' -> Punctuation,
    '!' -> Punctuation,
    '~' -> Punctuation,
    '`' -> Punctuation,
    ',' -> Punctuation,
    '\'' -> Punctuation,
    '"' -> Punctuation,
    '—' -> Punctuation, // long dash
    '’' -> Punctuation,
    '…' -> Punctuation,
    '“' -> Punctuation,
    '”' -> Punctuation,
    '/' -> Punctuation,
    //'-' -> Punctuation, // normal dash
    '?' -> Punctuation,
    '<' -> Punctuation,
    '>' -> Punctuation,
    '‘' -> Punctuation
  ).withDefaultValue(Other)

  private val apostophes = Set('\'', '’', '`', '‘')

  override def tokenize(sentence: String): Seq[Token] = {
    if (sentence.length == 0) {
      Seq()
    } else {
      var i: Int = 1
      var tokenStart: Int = 0
      var tokenCharacterClass: CharacterClass = characterClassMap(sentence.charAt(0))

      val result = new mutable.ArrayBuffer[Token](sentence.length / averageTokenLength)
      def addToken(start: Int, end: Int): Unit =
        result += Token(sentence.substring(start, end), tokenStart)

      while (i < sentence.length) {
        val c = sentence.charAt(i)
        val cl = characterClassMap(c)

        // special case for "they're", "isn't", etc.
        if (i >= 2 &&
          tokenCharacterClass == Punctuation &&
          cl == Other &&
          i - tokenStart == 1 &&
          characterClassMap(sentence.charAt(i - 2)) == Other &&
          apostophes.contains(sentence.charAt(tokenStart))) {
          tokenCharacterClass = Other
        }

        if (cl != tokenCharacterClass || cl == Punctuation) {
          if (tokenCharacterClass != Whitespace)
            addToken(tokenStart, i)
          tokenStart = i
          tokenCharacterClass = cl
        }
        i += 1
      }
      if (tokenStart < sentence.length && tokenCharacterClass != Whitespace) addToken(tokenStart, i)
      result
    }
  }
}

// special cases:
/*
e.g.
etc.

 */
