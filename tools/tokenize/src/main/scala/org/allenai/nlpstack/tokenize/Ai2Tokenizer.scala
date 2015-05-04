package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.{ Token, Tokenizer }
import org.apache.commons.lang.StringUtils

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

  // The code supports replacing one sequence with another (as long as the replacement sequence is
  // as long or shorter than the pattern sequence). In this version, the replacement sequence is
  // always of length 1, but hard-coding that assumption has no bearing on performance, and it
  // doesn't make the code shorter, so I left it this way.
  private val specialCases = IndexedSeq(
    IndexedSeq("'", "s") -> "'s", // He's doing the thing.
    IndexedSeq("'", "re") -> "'re", // You're doing the thing.
    IndexedSeq("'", "ll") -> "'ll", // I'll do the thing.
    IndexedSeq("'", "m") -> "'m", // I'm doing the thing.
    IndexedSeq("'", "d") -> "'d", // You'd do the same thing.
    IndexedSeq("'", "t") -> "'t", // I can't do the thing.
    IndexedSeq("'", "ve") -> "'ve", // I should've done the thing.
    IndexedSeq("e", ".", "g", ".") -> "e.g.",
    IndexedSeq("i", ".", "e", ".") -> "i.e.",
    IndexedSeq("a", ".", "m", ".") -> "a.m.",
    IndexedSeq("p", ".", "m", ".") -> "p.m.",
    IndexedSeq("et", ".", "al", ".") -> "et.al.",
    IndexedSeq("ph", ".", "d", ".") -> "ph.d.",
    IndexedSeq("mr", ".") -> "mr.",
    IndexedSeq("ms", ".") -> "ms.",
    IndexedSeq("m", ".", "sc", ".") -> "m.sc.",
    IndexedSeq("b", ".", "sc", ".") -> "b.sc.",
    IndexedSeq("b", ".", "a", ".") -> "b.a.",
    IndexedSeq("etc", ".") -> "etc.",
    IndexedSeq("u", ".", "s", ".") -> "u.s."
  ).map {
      case (pattern, replacement) =>
        pattern -> IndexedSeq(replacement)
    }

  require(specialCases.forall {
    case (pattern, replacement) =>
      pattern.map(_.length).sum == replacement.map(_.length).sum
  })

  // This is a requirement of the algorithm that uses specialCases, but it could be fixed relatively
  // easily.
  require(specialCases.forall {
    case (pattern, replacement) =>
      pattern.length >= replacement.length
  })

  // These are for "shouldn't", "couldn't", etc.
  private val shouldCouldWould = Set(
    "should",
    "could",
    "would",
    "must",
    "had",
    "did",
    "does",
    "is",
    "was",
    "has",
    "do",
    "have",
    "were",
    "are",
    "wo", // won't
    "ca" // can't
  ).map(_ + "n")

  override def tokenize(sentence: String): Seq[Token] = {
    if (sentence.length == 0) {
      Seq()
    } else {
      val normalized = sentence.toLowerCase.replaceAll("['’`‘]", "'")
      require(normalized.length == sentence.length)

      // tokenize by character classes
      var i: Int = 1
      var tokenStart: Int = 0
      var tokenCharacterClass: CharacterClass = characterClassMap(normalized.charAt(0))

      val result = new mutable.ArrayBuffer[Token](normalized.length / averageTokenLength)
      def addToken(start: Int, end: Int): Unit = {
        normalized.substring(start, end) match {
          case "cannot" | "gonna" =>
            result += Token(sentence.substring(start, start + 3), start)
            result += Token(sentence.substring(start + 3, end), start + 3)
          case _ =>
            result += Token(sentence.substring(start, end), start)
        }
      }

      while (i < normalized.length) {
        val c = normalized.charAt(i)
        val cl = characterClassMap(c)

        if (cl != tokenCharacterClass || cl == Punctuation) {
          if (tokenCharacterClass != Whitespace)
            addToken(tokenStart, i)
          tokenStart = i
          tokenCharacterClass = cl
        }
        i += 1
      }
      if (tokenStart < normalized.length && tokenCharacterClass != Whitespace) addToken(tokenStart, i)

      // glue together special cases
      val strings = result.map { t => normalized.substring(t.offset, t.offset + t.string.length) }
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
                strings.remove(j, pattern.length - replacement.length)
                replacement.zipWithIndex.foreach { case (r, k) => strings.update(j + k, r) }

                var offset = result(j).offset
                result.remove(j, pattern.length - replacement.length)
                replacement.zipWithIndex.foreach {
                  case (r, k) =>
                    result.update(
                      j + k,
                      Token(sentence.substring(offset, offset + r.length), offset)
                    )
                    offset += r.length
                }
              }
            }
          }
      }

      // glue together "*n't"
      var k = strings.length
      while (k >= 0) {
        k = strings.lastIndexOf("'t", k - 1)
        if (k >= 1 && shouldCouldWould.contains(strings(k - 1))) {
          // results(k - 1) is shouldn, couldn, etc.
          // results(k    ) is 't
          val previousToken = result(k - 1)
          val matchIsOneToken =
            previousToken.offset + previousToken.string.length == result(k).offset
          if (matchIsOneToken) {
            strings.update(k - 1, strings(k - 1).dropRight(1))
            strings.update(k, "n't")

            result.update(k - 1, Token(previousToken.string.dropRight(1), previousToken.offset))
            val replacementString =
              sentence.substring(result(k).offset - 1, result(k).offset + 2)
            result.update(k, Token(replacementString, result(k).offset - 1))
          }
        }
      }

      result
    }
  }
}
