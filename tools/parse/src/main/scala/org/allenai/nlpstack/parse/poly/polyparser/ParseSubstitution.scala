package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core._

import scala.annotation.tailrec

object ParseSubstitution {

  /** Takes a PolytreeParse, and maps its VariableTokens to specified PolytreeParses. */
  @tailrec
  def substitute(parse: PolytreeParse, substitutions: Map[Int, PolytreeParse]): PolytreeParse = {
    def findFirstVariable(parse: PolytreeParse): Option[(Int, Int)] = {
      val varPosition = parse.sentence.tokens indexWhere {
        case VariableToken(_) => true
        case _ => false
      }
      if (varPosition == -1) {
        None
      } else {
        parse.sentence.tokens(varPosition) match {
          case VariableToken(varIndex) => Some((varPosition, varIndex))
        }
      }
    }
    def adjustTokenIndex(tokIndex: Int, varPosition: Int, substitutionLength: Int): Int = {
      if (tokIndex < varPosition) {
        tokIndex
      } else {
        tokIndex + substitutionLength - 1
      }
    }
    def adjustSubtokenIndex(tokIndex: Int, varPosition: Int, substitutionLength: Int, zeroMapping: Int): Int = {
      if (tokIndex == 0) {
        adjustTokenIndex(zeroMapping, varPosition, substitutionLength)
      } else {
        tokIndex + varPosition - 1
      }
    }
    findFirstVariable(parse) match {
      case None => parse
      case Some((varPosition, varIndex)) =>
        val substitution = substitutions(varIndex)
        val substitutionLength = substitution.tokens.tail.size
        val revisedSentence: Sentence =
          Sentence(parse.sentence.tokens.take(varPosition) ++
            substitution.tokens.tail ++ parse.sentence.tokens.drop(varPosition + 1))
        val revisedBreadcrumb =
          (parse.breadcrumb.take(varPosition) map { crumb =>
            adjustTokenIndex(crumb, varPosition, substitutionLength)
          }) ++ (substitution.breadcrumb.tail map { crumb =>
            adjustSubtokenIndex(crumb, varPosition, substitutionLength, parse.breadcrumb(varPosition))
          }) ++ (parse.breadcrumb.drop(varPosition + 1) map { crumb =>
            adjustTokenIndex(crumb, varPosition, substitutionLength)
          })
        val revisedEdgeLabels =
          (parse.arcLabelByEndNodes map {
            case (key, value) =>
              (key map { x => adjustTokenIndex(x, varPosition, substitutionLength) }, value)
          }) ++
            (substitution.arcLabelByEndNodes map {
              case (endNodes, arcLabel) =>
                val adjustedEndNodes = endNodes map { endNode =>
                  adjustSubtokenIndex(endNode, varPosition,
                    substitutionLength, parse.breadcrumb(varPosition))
                }
                val adjustedArcLabel = if (arcLabel.toSymbol == 'ROOT) {
                  parse.breadcrumbArcLabel(varPosition)
                } else {
                  arcLabel
                }
                (adjustedEndNodes, adjustedArcLabel)
            })
        val revisedParse =
          PolytreeParse.easyInitialize(revisedSentence.tokens, revisedBreadcrumb, revisedEdgeLabels)
        substitute(revisedParse, substitutions)
    }
  }

  /** Constructs a Polytree Parse (with possible VariableTokens) from a string encoding.
    *
    * Two strings are required, the token string and the crumb string. An example is:
    *
    * token string: "$0 $1 equals|SYM $2 .|."
    * crumb string: "3|PREP 3|NARG 0|ROOT 3|NARG 3|PUNCT"
    *
    * Each token is either a variable (i.e. "$x" where x is a positive integer) or a part-of-speech
    * tagged word (i.e. "word|pos").
    *
    * The crumb string should contain a crumb corresponding to each token. Each crumb has the
    * form "crumb|arclabel", where crumb is the index of the token's breadcrumb and arclabel is
    * the label of the arc connecting the token to its crumb.
    *
    */
  def constructVariableParse(tokenStr: String, crumbStr: String): PolytreeParse = {
    if (tokenStr == "") {
      PolytreeParse.easyInitialize(Seq(NexusToken), IndexedSeq(-1), Map())
    } else {
      val tokenCodes: Seq[String] = tokenStr.split("\\s+").toSeq
      val tokens: Seq[Token] = NexusToken +: {
        tokenCodes map { code =>
          if (code.startsWith("$")) {
            new VariableToken(code.tail.toInt)
          } else {
            val splitCode = code.split("""\|""").toSeq
            Token.create(splitCode(0), finePos = Some(splitCode(1)),
              coarsePos = Some(WordClusters.ptbToUniversalPosTag(splitCode(1))))
          }
        }
      }
      val crumbCodes: IndexedSeq[String] = crumbStr.split("\\s+").toIndexedSeq
      val crumbs: IndexedSeq[Int] = -1 +: {
        crumbCodes map { code =>
          code.split("""\|""").head.toInt
        }
      }
      val edgeLabels: Map[Set[Int], ArcLabel] =
        (crumbCodes.zipWithIndex map {
          case (code, tokIndex) =>
            (Set(tokIndex + 1, crumbs(tokIndex + 1)), SingleSymbolArcLabel(Symbol(code.split("""\|""")(1))))
        }).toMap
      PolytreeParse.easyInitialize(tokens, crumbs, edgeLabels)
    }
  }
}
