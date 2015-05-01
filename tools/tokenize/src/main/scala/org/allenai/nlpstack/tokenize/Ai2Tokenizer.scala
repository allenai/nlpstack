package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.{ Token, Tokenizer }

import scala.collection.mutable

object Ai2Tokenizer extends Tokenizer {
  var averageTokenLength = 6 // low estimates are better

  private object CharacterClass extends Enumeration {
    type CharacterClass = Value
    val Whitespace, Punctuation, Dash, Number, Other = Value
  }
  import CharacterClass._
  private val characterClassMap = Map(
    ' ' -> Whitespace,
    '\n' -> Whitespace,
    '\r' -> Whitespace,
    '\t' -> Whitespace,
    '\u2028' -> Whitespace,
    '\u00A0' -> Whitespace,
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
    '–' -> Punctuation, // some other dash
    '’' -> Punctuation,
    '…' -> Punctuation,
    '“' -> Punctuation,
    '”' -> Punctuation,
    '/' -> Punctuation,
    '-' -> Dash, // normal dash
    '?' -> Punctuation,
    '<' -> Punctuation,
    '>' -> Punctuation,
    '‘' -> Punctuation,
    '•' -> Punctuation,
    '&' -> Punctuation,
    '‰' -> Punctuation,
    '|' -> Punctuation,
    '=' -> Punctuation,
    '~' -> Punctuation,
    '\\' -> Punctuation,
    '°' -> Punctuation
  ).withDefaultValue(Other)

  private val specialCases = Map(
    Seq("'", "s") -> "'s",
    Seq("'", "re") -> "'re",
    Seq("'", "ll") -> "'ll",
    Seq("'", "m") -> "'m", // I'm doing the thing.
    Seq("'", "d") -> "'d", // You'd do the same thing.
    Seq("e", ".", "g", ".") -> "e.g.",
    Seq("i", ".", "e", ".") -> "e.g.",
    Seq("a", ".", "m", ".") -> "a.m.",
    Seq("p", ".", "m", ".") -> "p.m.",
    Seq("etc", ".") -> "etc.",
    Seq("U", ".", "S", ".") -> "U.S."
  ).flatMap {
      case (pattern, replacement) =>
        val apostophes = Set('\'', '’', '`', '‘')
        if (pattern.head.startsWith("'") && replacement.startsWith("'")) {
          apostophes.map { a =>
            pattern.updated(0, a.toString) -> replacement.updated(0, a)
          }
        } else {
          Seq(pattern -> replacement)
        }
    }

  override def tokenize(sentence: String): Seq[Token] = {
    if (sentence.length == 0) {
      Seq()
    } else {
      // tokenize by character classes
      var i: Int = 1
      var tokenStart: Int = 0
      var tokenCharacterClass: CharacterClass = characterClassMap(sentence.charAt(0))

      val result = new mutable.ArrayBuffer[Token](sentence.length / averageTokenLength)
      def addToken(start: Int, end: Int): Unit =
        result += Token(sentence.substring(start, end), tokenStart)

      while (i < sentence.length) {
        val c = sentence.charAt(i)
        val cl = characterClassMap(c)

        if (cl != tokenCharacterClass || cl == Punctuation) {
          if (tokenCharacterClass != Whitespace)
            addToken(tokenStart, i)
          tokenStart = i
          tokenCharacterClass = cl
        }
        i += 1
      }
      if (tokenStart < sentence.length && tokenCharacterClass != Whitespace) addToken(tokenStart, i)

      // glue together special cases
      val strings = result.map(_.string.toLowerCase.replaceAll("['’`‘]", "'")) // TODO: try replacing apostrophes here
      specialCases.foreach {
        case (pattern, replacement) =>
          var j = strings.length
          while (j >= 0) {
            j = strings.lastIndexOfSlice(pattern, j - 1)
            if (j >= 0) {
              val matchStartOffset = result(j).offset
              val matchEndOffset =
                result(j + pattern.length - 1).offset +
                  result(j + pattern.length - 1).string.length
              val patternLength = pattern.map(_.length).sum
              val matchIsOneToken = (matchEndOffset - matchStartOffset) == patternLength
              if (matchIsOneToken) {
                strings.remove(j + 1, pattern.length - 1)
                strings.update(j, replacement)

                val replacementString =
                  sentence.substring(
                    result(j).offset,
                    result(j).offset + replacement.length
                  )
                val replacementToken = Token(replacementString, result(j).offset)
                result.remove(j + 1, pattern.length - 1)
                result.update(j, replacementToken)
              }
            }
          }
      }

      // glue together "*n't"
      var k = strings.length
      while (k >= 0) {
        k = strings.lastIndexOfSlice(Seq("'", "t"), k - 1)
        if (k >= 1 && strings(k - 1).endsWith("n")) {
          // results(k - 1) is don, won, shouldn, couldn, etc.
          // results(k    ) is '
          // results(k + 1) is t
          val previousToken = result(k - 1)
          val matchStartOffset = previousToken.offset
          val matchEndOffset = result(k + 1).offset + 1
          val matchIsOneToken =
            (matchEndOffset - matchStartOffset) == previousToken.string.length + 2
          if (matchIsOneToken) {
            strings.update(k - 1, strings(k - 1).dropRight(1))
            strings.update(k, s"n${strings(k)}t") // trying to put the same apostrophe back
            strings.remove(k + 1)

            result.update(k - 1, Token(previousToken.string.dropRight(1), previousToken.offset))
            val replacementString =
              sentence.substring(result(k).offset - 1, result(k).offset + 2)
            result.update(k, Token(replacementString, result(k).offset - 1))
            result.remove(k + 1)
          }
        }
      }

      result
    }
  }
}
