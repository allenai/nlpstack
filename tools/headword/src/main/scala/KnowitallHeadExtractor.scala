package org.allenai.nlpstack.headword

import org.allenai.nlpstack.core.{ Tokenizer, Postagger, PostaggedToken }
import org.allenai.nlpstack.core.headword.HeadExtractor

import collection.mutable.ArrayBuffer
import collection.immutable.HashSet

import edu.mit.jwi.item.{ POS, ISynset }

/** Extracts headwords from Postagged Token Sequences, e.g.
  * Head(good coffee)=coffee
  * Head(Barack Obama)=Barack Obama
  * Head(group of animals)=group
  * Head(10 policemen)=policemen
  * See argumentHead()
  *
  * It will do something similar for relation (verb) sequences.
  * See relationHead()
  *
  * Requires wordnet to exist somewhere on the local filesystem.
  */
class KnowitallHeadExtractor(
    wordnetHome: String = JwiTools.defaultWordnetHome
) extends HeadExtractor {

  val jwiTools = new JwiTools(wordnetHome)

  private val relationStopWords = HashSet("has", "have", "had", "did", "do")
  private val relationStopTags = HashSet("MD", "JJ", "JJR", "JJS", "RB", "RBR", "RBS", "CC",
    "UH", "PRP", "PRP$", "DT", "WP", "WP$", "WRB", "CD")

  /** Given a string representing a relation, will attempt to extract the tokens representing the
    * headword tokens of the relation.
    */
  def relationHead(
    tokenizer: Tokenizer, postagger: Postagger
  )(relation: String): Seq[PostaggedToken] = {
    val tokens = postagger.postag(tokenizer)(relation)
    relationHead(tokens)
  }

  /** Given a Seq[PostaggedTokens] will attempt to extract the tokens representing the
    * headword tokens of the relation.
    */
  def relationHead(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val outTokens = tokens.filter(token => !(relationStopTags.contains(token.postag) ||
      relationStopWords.contains(token.string)))
    outTokens
  }

  /** Given a string representing an argument, will attempt to extract the tokens representing the
    * headword tokens.
    */
  def argumentHead(
    tokenizer: Tokenizer, postagger: Postagger
  )(argument: String): Seq[PostaggedToken] = {
    val tokens = postagger.postag(tokenizer)(argument)
    argumentHead(tokens)
  }

  /** Given a Seq[PostaggedTokens] will attempt to extract the tokens representing the
    * headword tokens.
    */
  def argumentHead(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {

    var subTokens = truncateBeforeRelativeClause(tokens)

    var returnTokens = findNPofNP(subTokens)
    if (!returnTokens.isEmpty) {
      subTokens = returnTokens
    }

    returnTokens = removeTokensBeforeAppositive(subTokens)
    if (!returnTokens.isEmpty) {
      subTokens = returnTokens
    }

    returnTokens = removeTokensAfterPunctuation(subTokens)
    if (!returnTokens.isEmpty) {
      subTokens = returnTokens
    }

    returnTokens = removeTokensAfterConjunctionsOrPrepositions(subTokens)
    if (!returnTokens.isEmpty) {
      subTokens = returnTokens
    }

    def allowedToken(p: PostaggedToken) = !p.isWhWord &&
      (p.isNoun || p.isAdjective || p.isVerbGerund || p.isCardinalNumber ||
        p.isDeterminer || Postagger.articles.contains(p.string))

    def contentToken(p: PostaggedToken): Boolean = p.isNoun || p.isPronoun || p.isCardinalNumber

    val truncateIndex = subTokens.indexWhere(p => !allowedToken(p))
    if (truncateIndex > 0 && subTokens.take(truncateIndex)
      .find(p => (p.isPronoun || p.isNoun)) != None) {
      subTokens = subTokens.take(truncateIndex)
    } else if (subTokens.find(p => contentToken(p)) == None) {
      return Nil
    }

    // If NNPS* NNPS* -> return full sequence. (Saudi Arabia --> Saudi Arabia)
    // If NNPS* NN  -> return last NN (Saudi exile -> exile)
    // If NN NN -> return last NN (air plane --> air plane)
    return findLastNounAndNormalized(subTokens).getOrElse(Nil)
  }

  //
  // private helper functions
  //

  private def isConjunction(token: PostaggedToken) = {
    token.isCoordinatingConjunction || token.isWhWord || Postagger.whWords.contains(token.string)
  }

  private def removeTokensAfterConjunctionsOrPrepositions(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    var firstConjPrepIndex = tokens.indexWhere(token => isConjunction(token) || token.isPreposition)
    var firstNounIndex = tokens.indexWhere(token => token.isNoun)
    if (firstConjPrepIndex > 0 && firstNounIndex < firstConjPrepIndex) {
      tokens.take(firstConjPrepIndex)
    } else {
      tokens
    }
  }

  private def truncateBeforeRelativeClause(intokens: Seq[PostaggedToken]) = {
    intokens.takeWhile(tok => !tok.isWhWord)
  }

  private def removeTokensBeforeAppositive(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val apostropheIndex = tokens.indexWhere(token => token.string.equals("POS") || token.postag.equals("POS"))
    if (apostropheIndex > 0 && (apostropheIndex + 1) < tokens.size) {
      tokens.drop(apostropheIndex + 1)
    } else {
      tokens
    }
  }

  private val alphaNumDotDollarRe = """[^a-zA-Z0-9.$]""".r
  private def removeTokensAfterPunctuation(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {
    val puncIndex = tokens.indexWhere(token => alphaNumDotDollarRe.findFirstIn(token.string) == None)
    if (puncIndex > 0) {
      tokens.take(puncIndex)
    } else {
      tokens
    }
  }

  private val leadingModPatterns = """^(DT|CD|(DT*) JJ|JJ|RBS) of""".r

  private def findNPofNP(tokens: Seq[PostaggedToken]): Seq[PostaggedToken] = {

    val leadingOfIndex = tokens.indexWhere(token => token.string.equals("of") || token.string.equals("in"))
    if (leadingOfIndex > 0) {
      // First step is to find the head noun of an argument phrase, as follows:
      // If the phrase begins with "NP of NP", check the WN type and hypernyms of the first NP.
      // If the first NP has type { number[n2], group[n1], quantity[n1], part[n1], amount[n1] },
      // then remove "NP of".
      // If the phrase begins with "Adj of NP", then remove "Adj of NP".
      // (example:"some of NP", "any of NP").
      // Find first token of remaining phrase that is not in { Noun, Adj, Determiner}.
      // Truncate phrase at that word.
      val posTokenString = tokens.take(leadingOfIndex + 1).map(token => {
        if (token.string.equals("of") || token.string.equals("in")) "of" else token.postag
      }).mkString(" ")

      if (leadingModPatterns.findFirstIn(posTokenString) != None) {
        return tokens.drop(leadingOfIndex + 1)
      }

      var leadingNPs = tokens.take(leadingOfIndex).filter(x => x.isNoun)
      var trailingNPs = tokens.drop(leadingOfIndex + 1).filter(x => x.isNoun)
      if (leadingNPs.find(x => !x.isProperNoun) == None) {
        return tokens
      }
      leadingNPs.filter(x => !x.isProperNoun).foreach(x =>
        if (WordNetUtil.isGroupQuantityAmountNumberOrPart(x)) return trailingNPs)
      return leadingNPs
    }
    return tokens
  }

  private def findLastNounAndNormalized(subTokens: Seq[PostaggedToken]): Option[Seq[PostaggedToken]] = {

    assert(subTokens.size > 0, "Sub tokens cannot be zero.")
    //If only one token left return the stemmed version.

    //If no noun exists return the stemmed version of the entire string.
    if (subTokens.find(token => (token.isNoun || token.isPronoun)) == None) {
      return Some(Seq(subTokens.last))
    }

    //If there is only one noun left. Return.
    if (subTokens.size < 2) {
      return Some(subTokens)
    }

    def findLastPosSequence(tokens: Seq[PostaggedToken], posTester: PostaggedToken => Boolean): Option[Seq[PostaggedToken]] = {
      val nounSequence = tokens.zipWithIndex.filter(x => posTester(x._1)).reverse.toSeq
      if (nounSequence.isEmpty) return None
      var prevIndex = nounSequence.head._2
      val lastSeq = new ArrayBuffer[PostaggedToken]
      nounSequence.foreach(n => {
        val curIndex = n._2
        if ((prevIndex - curIndex) > 1) {
          return Some(lastSeq.reverse)
        }
        prevIndex = curIndex
        lastSeq += n._1
      })
      return Some(lastSeq.reverse)
    }
    def isNoun(x: PostaggedToken) = x.isNoun || x.isPronoun
    def isProperNoun(x: PostaggedToken) = x.isProperNoun

    findLastPosSequence(subTokens, isNoun) match {
      case Some(lastNounSeq: Seq[PostaggedToken]) => {
        val lastNoun = lastNounSeq.last
        if (lastNoun.isCommonNoun) {
          return Some(Seq(lastNoun))
        } else {
          return findLastPosSequence(lastNounSeq, isProperNoun)
        }
      }
      case _ => None
    }
  }

  /** Uses JwiTools to determine if a PostaggedToken isGroupQuantityAmountNumberOrPart
    */
  object WordNetUtil {
    var types: Set[ISynset] = HashSet[ISynset]()
    var typeSynsetIds: Set[String] = HashSet[String]()

    val Xclasses = Seq[(String, Int)](
      ("number", 1),
      ("group", 0),
      ("quantity", 0),
      ("part", 0),
      ("amount", 0),
      ("percentage", 0),
      ("proportion", 3)
    )

    Xclasses.foreach {
      case (word, sense) => {
        val stemmedWord = jwiTools.stem(word, 0)
        val indexedWord = jwiTools.wordnetDict.getIndexWord(stemmedWord, POS.NOUN)
        val wordIDs = indexedWord.getWordIDs
        val dictWord = wordIDs.get(sense)
        val synset = jwiTools.wordnetDict.getWord(dictWord).getSynset
        types += synset
        typeSynsetIds += synset.getID.toString
      }
    }

    val XclassNames = HashSet(Xclasses.map(xc => xc._1): _*)

    def isGroupQuantityAmountNumberOrPart(token: PostaggedToken, sense: Int = 0): Boolean = {

      if (XclassNames.contains(token.string)) return true

      val hypernyms: Seq[Set[ISynset]] = jwiTools.posTokensToHypernymStream(Seq(token), sense)

      hypernyms.iterator.filter(st => st.size > 0).foreach(st => {
        st.foreach(s => if (typeSynsetIds.contains(s.getID.toString)) { return true })
      })
      return false
    }
  }
}