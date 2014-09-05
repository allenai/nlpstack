package org.allenai.nlpstack.core

import java.util.regex.Matcher

/** Shared utilities for making Factorie work. These are probably not generally
  * useful.
  */
object FactorieUtilities {
  // Factorie's tokenizer crashes on unclosed XML tags. To work around this, we
  // detect unclosed tags, and replace the opening < with a space.
  private val unclosedTagRegex = "<([^>]{100})".r
  def replaceUnclosedTag(s: String): String = {
    val replaced = unclosedTagRegex.replaceAllIn(s, m => Matcher.quoteReplacement(" " + m.group(1)))
    // Have to do this repeatedly for the case of "foo << barbarbarbar..."
    if (replaced == s) s else replaceUnclosedTag(replaced)
  }
}
