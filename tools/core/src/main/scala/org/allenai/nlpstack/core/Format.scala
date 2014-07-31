package org.allenai.nlpstack.core

import scala.util.Try

trait Writer[F, T] {
  def write(from: F): T
}

trait Reader[F, T] {
  def read(from: F): T
  def readTry(from: F): Try[T] = Try(this.read(from))
}

trait Format[F, T] extends Writer[F, T] with Reader[T, F] {
  def roundtrip(f: F) = read(write(f))
  def reverseRoundtrip(t: T) = write(read(t))
}

object Format {
  object stringQuoter extends Quoter(Set('"'))

  class Quoter(val chars: Set[Char]) {
    def this(charString: String) = this(charString.toSet)

    def quote(s: String): String = {
      val escapedBackslashes = s.replace("\\", "\\\\")
      chars.foldLeft(escapedBackslashes)((unreplaced: String, char: Char) =>
        unreplaced.replace(char.toString, "\\" + char))
    }

    def unquote(s: String): String = {
      val escapedBackslashes = chars.foldLeft(s)((quoted: String, char: Char) =>
        quoted.replace("\\" + char, char.toString))
      escapedBackslashes.replace("\\\\", "\\")
    }
  }
}
