package org.allenai.nlpstack.parse.poly.core

object WordClusters {

  val prefixes = Set('anti, 'de, 'dis, 'en, 'em, 'fore, 'in, 'im, 'il, 'ir, 'inter, 'mid,
    'mis, 'non, 'over, 'pre, 're, 'semi, 'sub, 'super, 'trans, 'un, 'under)

  val suffixes = Set('able, 'ible, 'al, 'ial, 'ed, 'en, 'er, 'est, 'ful, 'ic, 'ing, 'ion,
    'tion, 'ation, 'ition, 'ity, 'ty, 'ive, 'ative, 'itive, 'less, 'ly, 'ment, 'ness, 'ous,
    'eous, 'ious, 's, 'es, 'y)

  val stopWords = Set('a, 'about, 'above, 'after, 'again, 'against, 'all, 'am, 'an, 'and, 'any,
    'are, Symbol("n't"), 'as, 'at, 'be, 'because, 'been, 'before, 'being, 'below, 'between,
    'both, 'but, 'by, 'can, 'cannot, 'could, 'did, 'do, 'does, 'doing, 'down, 'during,
    'each, 'few, 'for, 'from, 'further, 'had, 'has, 'have, 'having, 'he, Symbol("'d"),
    Symbol("'ll"), Symbol("'s"), 'her, 'here, 'hers, 'herself, 'him, 'himself, 'his, 'how,
    'i, Symbol("'m"), Symbol("'ve"), 'if, 'in, 'into, 'is, 'it, 'its, 'itself, 'let, 'me,
    'more, 'most, 'must, 'my, 'myself, 'no, 'nor, 'not, 'of, 'off, 'on, 'once, 'only, 'or,
    'other, 'ought, 'our, 'ours, 'ourselves, 'out, 'over, 'own, 'same, 'she, 'should, 'so,
    'some, 'such, 'than, 'that, 'the, 'their, 'theirs, 'them, 'themselves, Symbol("then"),
    'there, 'these, 'they, Symbol("'re"), 'this, 'those, 'through, 'to, 'too, 'under, 'until,
    'up, 'very, 'was, 'we, 'were, 'what, 'when, 'where, 'which, 'while, 'who, 'whose, 'whom,
    'why, 'with, 'wo, 'would, 'you, 'your, 'yours, 'yourself, 'yourselves)

  val puncWords = Set(".", ",", ":", "--", "?", "$", "-LRB-", "(", ")", "-RRB-", "'", ";", "``",
    "''", "%", "&") map { Symbol(_) }

  val commonWords = Set("$", "%", "&", "'", "'re", "'s", ",", "--", ".",
    "1", "1/2", "10", "1987", "1990", "2", "3/4", "30", "40", ":", ";", "``", "a", "about",
    "added", "after", "against", "ago", "all", "almost", "also", "among", "an", "and", "are",
    "as", "at", "average", "back", "be", "because", "become", "been", "before", "being",
    "between", "both", "but", "buy", "by", "called",
    "close", "compared", "concern", "continue", "did",
    "does", "dollar", "down", "due", "during", "earlier", "even", "expected", "few",
    "for", "from", "get", "had", "has", "have", "his", "if",
    "in", "including", "increase", "index", "into", "is", "it", "just", "least",
    "like", "likely", "little", "lot", "major", "make", "may",
    "month", "more", "most", "much", "must", "n't", "net", "new", "next", "no", "not", "now",
    "of", "off", "old", "on", "one", "only", "or", "out",
    "people", "put",
    "rose", "said", "say", "says", "see",
    "set", "since", "so", "sold", "some", "still",
    "such", "take", "than", "that", "the", "them", "then", "this", "those", "through", "time",
    "to", "too", "two", "under", "unit", "until", "up", "use", "used",
    "was", "we", "well", "were", "what", "where", "whether", "which", "while", "who", "will",
    "with", "year", "yesterday") map { Symbol(_) }

  val keyWords = stopWords ++ puncWords ++ commonWords

  /** Maps standard Penn Treebank-style part-of-speech tags into the Google's "universal" POS set:
    *
    * https://code.google.com/p/universal-pos-tags/
    *
    * TODO: move this to the nlpstack.postag library
    */
  val ptbToUniversalPosTag: Map[String, String] = {
    Map("!" -> ".", "#" -> ".", "$" -> ".", "''" -> ".", "(" -> ".", ")" -> ".", "," -> ".",
      "-LRB-" -> ".", "-RRB-" -> ".", "." -> ".", ":" -> ".", "?" -> ".", "CC" -> "CONJ",
      "CD" -> "NUM", "CD|RB" -> "X", "DT" -> "DET", "EX" -> "DET", "FW" -> "X", "HYPH" -> ".",
      "IN" -> "ADP", "IN|RP" -> "ADP", "JJ" -> "ADJ", "JJR" -> "ADJ", "JJRJR" -> "ADJ",
      "JJS" -> "ADJ", "JJ|RB" -> "ADJ", "JJ|VBG" -> "ADJ", "LS" -> "X", "MD" -> "VERB",
      "NN" -> "NOUN", "NNP" -> "NOUN", "NNPS" -> "NOUN",
      "NNS" -> "NOUN", "NN|NNS" -> "NOUN", "NN|SYM" -> "NOUN",
      "NN|VBG" -> "NOUN", "NP" -> "NOUN", "PDT" -> "DET", "POS" -> "PRT", "PRN" -> ".",
      "PRP" -> "PRON",
      "PRP$" -> "PRON", "PRP|VBP" -> "PRON", "PRT" -> "PRT", "RB" -> "ADV", "RBR" -> "ADV",
      "RBS" -> "ADV", "RB|RP" -> "ADV", "RB|VBG" -> "ADV", "RN" -> "X", "RP" -> "PRT",
      "SYM" -> "X", "TO" -> "PRT", "UH" -> "X", "VB" -> "VERB", "VBD" -> "VERB",
      "VBD|VBN" -> "VERB", "VBG" -> "VERB", "VBG|NN" -> "VERB", "VBN" -> "VERB",
      "VBP" -> "VERB", "VBP|TO" -> "VERB", "VBZ" -> "VERB", "VP" -> "VERB", "WDT" -> "DET",
      "WH" -> "X", "WP" -> "PRON", "WP$" -> "PRON", "WRB" -> "ADV", "``" -> ".")
  }

  /** Given a list of strings, creates a histogram that maps the strings to their frequency in
    * the list.
    *
    * @param words the strings that we want to count
    * @return a mapping from strings to their frequency in the argument list
    */
  def wordFrequencies(words: List[String]): Map[String, Int] = {
    words.groupBy(x => x) map { case (str, vals) => (str, vals.size) }
  }

  /** Given a list of strings and a minimum threshold `qualifyingCount`, returns the set of
    * strings that appear at least `qualifyingCount` times in the argument list.
    *
    * @param words the strings that we want to count
    * @param qualifyingCount the minimum frequency of words to include in the return value
    * @return the set of strings that appear at least `qualifyingCount` times in `words`
    */
  def harvestFrequentWords(words: List[String], qualifyingCount: Int): Set[String] = {
    (wordFrequencies(words) filter { case (word, freq) => freq >= qualifyingCount }).keys.toSet
  }
}
