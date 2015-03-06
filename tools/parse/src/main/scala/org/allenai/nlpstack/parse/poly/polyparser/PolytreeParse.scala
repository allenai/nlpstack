package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ File, PrintWriter }
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm.{ Sculpture, MarbleBlock }

import scala.annotation.tailrec
import scala.io.Source
import spray.json.DefaultJsonProtocol._
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.nlpstack.core.{ Token => NLPStackToken }
import org.allenai.nlpstack.core.Tokenizer
import org.allenai.nlpstack.postag.defaultPostagger

/** A PolytreeParse is a polytree-structured dependency parse. A polytree is a directed graph
  * whose undirected structure is a tree. The nodes of this graph will correspond to an indexed
  * sequence of tokens (think the words from a sentence), whose zeroth element is a reserved
  * 'nexus' token which does not correspond to a word in the original sentence. The nexus must be
  * one of the roots of the directed graph (i.e. it cannot be the child of any node).
  *
  * Since the undirected structure is a tree, every node (other than the nexus) has a unique
  * neighbor which is one step closer to the nexus than itself (this may be the nexus itself).
  * This neighbor is referred to as the node's 'breadcrumb'.
  *
  * It has four major fields:
  * - `tokens` is a vector of Token objects (in the order that they appear in the associated
  * sentence). The zeroth element is assumed to be the nexus.
  * - `breadcrumb` tells you the unique neighbor that is closer to the nexus in the
  * undirected tree (this can be the nexus itself); for instance, if breadcrumb(5) = 3,
  * then token 3 is one step closer to the nexus from token 5. The breadcrumb of the nexus
  * should be -1.
  * - `children` tells you the set of children of a node in the polytree; for instance, if
  * children(5) = Set(3,6,7), then token 5 has three children: tokens 3, 6, and 7
  * - `arclabels` tells you the labeled neighbors of a node in the undirected tree; for instance,
  * if arclabels(5) = Set((4, 'det), (7, 'amod)), then token 5 has two neighbors, reached with
  * arcs labeled 'det and 'amod (the labels are scala Symbol objects)
  *
  * @param sentence the parsed sentence (the zeroth token of which should be the nexus)
  * @param breadcrumb the breadcrumb of each token (see above definition)
  * @param children the set of children of each token in the polytree
  * @param arclabels the set of labeled neighbors of each token in the undirected tree
  */
case class PolytreeParse(
    val sentence: Sentence,
    val breadcrumb: Vector[Int],
    val children: Vector[Set[Int]],
    val arclabels: Vector[Set[(Int, Symbol)]]
) extends MarbleBlock with Sculpture {
  require(breadcrumb(0) == -1)
  require(sentence.size == breadcrumb.size)
  require(sentence.size == children.size)
  require(sentence.size == arclabels.size)

  @transient
  val tokens: Seq[Token] = sentence.tokens

  /** Return the set of gretels for a particular token.
    *
    * Definition: if x is the breadcrumb of y, then y is a gretel of x.
    *
    * @param token the token (index) for which we want to identify the gretels
    * @return the gretels of the specified token
    */
  def getGretels(token: Int): Set[Int] = {
    gretels.getOrElse(token, Vector[Int]()).toSet
  }

  /** If x is the breadcrumb of y, then y is a gretel of x. */
  @transient lazy val gretels: Map[Int, Vector[Int]] = breadcrumb.zipWithIndex groupBy
    { _._1 } mapValues { x => x map { _._2 } }

  def getParents(): Map[Int, Seq[Int]] = {
    (for {
      (x, y) <- children.zipWithIndex
      z <- x
    } yield (z, y)) groupBy (_._1) mapValues { x => (x map { _._2 }).toSeq.sorted }
  }

  /** Maps a set of token indices (the set should contain exactly two elements) to the label
    * of the arc that connects them (regardless of directionality).
    */
  @transient
  val arcLabelByEndNodes: Map[Set[Int], Symbol] = {
    (for {
      (labeledNeighbors, node1) <- arclabels.zipWithIndex
      (node2, label) <- labeledNeighbors
    } yield (Set(node1, node2) -> label)).toMap
  }

  /** Maps each token index to the arclabel between itself and its breadcrumb. */
  @transient
  lazy val breadcrumbArcLabel: Vector[Symbol] = {
    'NEXUS +: (breadcrumb.zipWithIndex.tail map {
      case (crumb, index) =>
        arcLabelByEndNodes(Set(crumb, index))
    })
  }

  @transient lazy val families: Seq[Seq[Int]] = {
    Range(0, tokens.size) map { tokIndex =>
      tokIndex +: children(tokIndex).toSeq.sorted
    }
  }

  @transient lazy val labeledFamilies: Seq[(Int, Seq[(Symbol, Int)])] = {
    Range(0, tokens.size) map { tokIndex =>
      (tokIndex, children(tokIndex).toSeq.sorted map { child =>
        (arcLabelByEndNodes(Set(tokIndex, child)), child)
      })
    }
  }

  @transient
  lazy val siblings: Vector[Set[Int]] = {
    breadcrumb.zipWithIndex map {
      case (node, nodeIndex) =>
        gretels(node).toSet - nodeIndex
    }
  }

  /** Returns whether the two argument token indices have a connecting arc (regardless of
    * directionality) in the polytree.
    *
    * @param tokenIndex1 the first token index
    * @param tokenIndex2 the second token index
    * @return true iff the argument token indices have a connecting arc in the polytree
    */
  def areNeighbors(tokenIndex1: Int, tokenIndex2: Int): Boolean = {
    // because the underlying structure is a tree (and thus there is a single path between any
    // two nodes, including the nexus), two nodes are connected iff one of them is the
    // other's breadcrumb
    (breadcrumb(tokenIndex1) == tokenIndex2) || (breadcrumb(tokenIndex2) == tokenIndex1)
  }

  /** Returns the sequence of breadcrumbs en route to the nexus from the argument node. This
    * sequence is in order of proximity to the nexus (i.e. the node's breadcrumb is the
    * final element of the list, and the nexus is the first element of the list).
    *
    * @param tokenIndex the node of interest
    * @param soFar recursively constructed return value (for use by tail recursion)
    * @return the sequence of breadcrumbs en route from the nexus to the argument node
    */
  @tailrec
  private def getPath(tokenIndex: Int, soFar: Seq[Int] = Seq()): Seq[Int] = {
    breadcrumb(tokenIndex) match {
      case -1 => soFar
      case crumb => getPath(crumb, crumb +: soFar)
    }
  }

  /** The nth element of this vector is the "path" of the nth node (see getPath()). */
  @transient
  lazy val paths: Vector[Seq[Int]] = {
    Range(0, breadcrumb.size).toVector map (getPath(_))
  }

  @transient
  lazy val depthFirstPreorder: Iterable[Int] = depthFirstPreorderHelper(0)

  private def depthFirstPreorderHelper(root: Int): Iterable[Int] = {
    val gretelOrders = gretels.getOrElse(root, Vector()) map { gretelIndex =>
      depthFirstPreorderHelper(gretelIndex)
    }
    root +: gretelOrders.flatten
  }

  @transient
  lazy val asConllX: String = {
    val lines: Vector[String] = breadcrumb.zipWithIndex.tail map {
      case indexedCrumb => {
        val crumb: Int = indexedCrumb._1
        val index: Int = indexedCrumb._2
        List(index, tokens(index).word.name, "_",
          tokens(index).getDeterministicProperty('cpos) match {
            case Token.propertyNotFound => "_"
            case x => x.name
          },
          tokens(index).getDeterministicProperty('pos) match {
            case Token.propertyNotFound => "_"
            case x => x.name
          },
          "_", crumb,
          arcLabelByEndNodes(Set(crumb, index)).name,
          "_", "_").mkString("\t")
      }
    }
    lines.mkString("\n")
  }

  /** Converts the dependency parse into a constituency parse (i.e. a tree for which the leaves,
    * rather than the entire node set, are labeled with the words of a sentence).
    *
    * It does so by making each node sprout a child. This child is labeled with its parent's
    * word. Then its parent is relabeled with the label of the arc leading to its breadcrumb.
    */
  @transient lazy val asConstituencyParse: PositionTree = {
    val allNodes: IndexedSeq[DirectedGraphNode] = {
      val internalNodes: Seq[DirectedGraphNode] = Range(1, breadcrumb.size) map { index =>
        DirectedGraphNode(Map(
          ConstituencyParse.constituencyLabelName -> breadcrumbArcLabel(index).name
        ))
      }
      val leafNodes: Seq[DirectedGraphNode] = Range(1, breadcrumb.size) map { index =>
        DirectedGraphNode(Map(
          ConstituencyParse.wordLabelName -> sentence.tokens(index).word.name,
          ConstituencyParse.constituencyLabelName -> ConstituencyParse.headLabel
        ))
      }
      val nexusNode: DirectedGraphNode =
        DirectedGraphNode(Map(ConstituencyParse.constituencyLabelName ->
          ConstituencyParse.nexusLabel))
      nexusNode +:
        ((internalNodes zip leafNodes) flatMap { case (x, y) => List(x, y) }).toIndexedSeq
    }
    val allEdges: IndexedSeq[Seq[DirectedGraphEdge]] = {
      val internalEdges: Seq[DirectedGraphEdge] = Range(1, breadcrumb.size) map { index =>
        DirectedGraphEdge(
          math.max(0, (2 * breadcrumb(index)) - 1),
          (2 * index) - 1, Map()
        )
      }
      val leafEdges: Seq[DirectedGraphEdge] = Range(1, breadcrumb.size) map { index =>
        DirectedGraphEdge(
          (2 * index) - 1,
          2 * index, Map()
        )
      }
      val edgeMap = (internalEdges ++ leafEdges) groupBy { edge => edge.from }
      Range(0, allNodes.size) map { nodeIndex =>
        (edgeMap.getOrElse(nodeIndex, Seq()) sortBy { edge => edge.to })
      }
    }
    val digraph = DirectedGraph(allNodes, allEdges)
    val rootNode = digraph.getOutgoingEdges(0).head.to
    digraph.toPositionTree(rootNode)
  }

  @transient
  lazy val relativeCposMap: Map[Int, ((Boolean, Symbol), Int)] = {
    relativeCposMapHelper(depthFirstPreorder, Map())
  }

  @tailrec
  private def relativeCposMapHelper(
    nodesToProcess: Iterable[Int],
    result: Map[Int, ((Boolean, Symbol), Int)]
  ): Map[Int, ((Boolean, Symbol), Int)] = {

    nodesToProcess.headOption match {
      case None => result
      case Some(nodeIndex) =>
        val myGretels: Vector[Int] = gretels.getOrElse(nodeIndex, Vector[Int]())
        val myGretelLabels = myGretels map { gretel =>
          (gretel < nodeIndex, tokens(gretel).getDeterministicProperty('cpos))
        }
        val myGretelLabelFreq = myGretelLabels.zipWithIndex map {
          case (gretelLabel, index) =>
            myGretelLabels.take(index).count(_ == gretelLabel)
        }
        relativeCposMapHelper(
          nodesToProcess.tail,
          result ++ (myGretels zip (myGretelLabels zip myGretelLabelFreq)).toMap
        )
    }
  }

  override def toString(): String = {
    (labeledFamilies map {
      case (node, labeledChildren) =>
        (s"${sentence.tokens(node).word.name}[$node]" +: (labeledChildren map {
          case (arclabel, familyMember) =>
            s"${arclabel.name}.${sentence.tokens(familyMember).word.name}[$familyMember]"
        })).mkString(":")
    }).mkString(" ")
  }

}
object PolytreeParse {

  def fromFile(filename: String, fileFormat: PolytreeParseFileFormat): Iterator[PolytreeParse] = {
    fileFormat match {
      case conllx: ConllX => fromConllX(filename, conllx.useGoldPOSTags, conllx.makePoly)
      case x => throw new ClassCastException(
        s"File format $x is not supported by PolytreeParse.fromFile"
      )
    }
  }

  /** Creates an Iterator over PolytreeParse objects from a CoNLL-X format file, which is a tab-
    * separated format with one line per sentence word, and 10 fields per line, like this:
    *
    * 1 The _ DET DT _ 2 DET _ _
    * 2 cat _ NOUN NN _ 3 NSUBJ _ _
    * 3 sat _ VERB VBD _ 0 ROOT _ _
    * 4 by _ PREP IN _ 3 PREP _ _
    * 5 me _ NOUN PRN _ 4 POBJ _ _
    * 6 . _ PUNC . _ 3 . _ _
    *
    * The ten fields are (sentence position, word, lemma, coarse POS tag, fine POS tag, IGNORED,
    * breadcrumb position, arc label, IGNORED, IGNORED). If a field is not specified, then a
    * single underscore is inserted.
    *
    * Different parse trees within the same file are separated by a single blank line.
    *
    * @param filename the CoNLL-X file containing the dependency parses
    * @param useGoldPosTags if true, then use the (fine) POS tags contained in the file (otherwise,
    * use an automatic tagger)
    * @return an Iterator over dependency parses contained in the argument file
    */
  def fromConllX(filename: String, useGoldPosTags: Boolean,
    makePoly: Boolean): Iterator[PolytreeParse] = {

    val rawIter =
      fromConllHelper(Source.fromFile(filename).getLines, useGoldPosTags = useGoldPosTags).iterator
    if (makePoly) {
      rawIter map { x => PolytreeParse.arcInverterStanford(x) }
    } else {
      rawIter
    }
  }

  /** Iterates over parses and writes each parse to the specified file (in Conll-X format).
    *
    * @param outputFilename where to direct the parses
    * @param parses the parse objects to iterate over
    */
  def writeParsesAsConllX(
    outputFilename: String,
    parses: Iterator[Option[PolytreeParse]]
  ): Unit = {

    val writer = new PrintWriter(new File(outputFilename))
    try {
      parses foreach { optParse =>
        optParse match {
          case Some(parse) => writer.println(parse.asConllX + "\n")
          case None => writer.println("FAIL\n")
        }
      }
    } finally {
      writer.close()
    }
  }

  private def fromConllHelper(
    fileLines: Iterator[String],
    useGoldPosTags: Boolean
  ): Stream[PolytreeParse] = {

    val (nextParseLines, rest) = fileLines.span(_.nonEmpty)
    constructFromConllString(nextParseLines, useGoldPosTags) #:: (if (rest.hasNext) {
      rest.next()
      if (rest.hasNext) {
        // check for more in case last line in file is newline
        fromConllHelper(rest, useGoldPosTags)
      } else {
        Stream.empty
      }
    } else {
      Stream.empty
    })
  }

  private def constructFromConllString(
    conllLines: Iterator[String],
    useGoldPosTags: Boolean
  ): PolytreeParse = {

    val rows: List[Array[String]] = (for {
      line <- conllLines
    } yield line.split("\t")).toList

    // For the following, note that each row has 10 elements. The relevant elements are:
    // - row(0) is the word position (in the sentence)
    // - row(1) is the word
    // - row(4) is the fine POS tag
    // - row(6) is the breadcrumb
    // - row(7) is the arc label (for the unique arc between the word and its breadcrumb)
    val iFinePos = 4
    val sentence =
      Sentence(
        (NexusToken +: (rows map { row =>
        Token(
          Symbol(row(1)),
          Token.createProperties(
            row(1),
            goldCpos = if (useGoldPosTags) {
              WordClusters.ptbToUniversalPosTag.get(row(iFinePos))
            } else {
              None
            },
            goldPos = if (useGoldPosTags) {
              Some(row(iFinePos))
            } else {
              None
            }
          )
        )
      })).toVector
      )
    val breadcrumbPos: Int = 6
    val arcLabelPos: Int = 7
    val breadcrumb: Vector[Int] = (-1 +: rows.map(row => row(breadcrumbPos).toInt)).toVector
    val childMap: Map[Int, Set[Int]] = ((breadcrumb.zipWithIndex.tail groupBy { _._1 })
      map { case (key, value) => (key, (value map { _._2 }).toSet) })
    val children: Vector[Set[Int]] = ((0 to (sentence.tokens.size - 1)) map { x =>
      childMap.getOrElse(x, Set())
    }).toVector
    val neighbors: Vector[Set[Int]] = ((0 to (sentence.tokens.size - 1)) map { i =>
      childMap.getOrElse(i, Set()) + breadcrumb(i)
    }).toVector
    val arcLabelByTokenPair: Map[Set[Int], Symbol] = rows.map(row => (Set(
      row(0).toInt,
      row(breadcrumbPos).toInt
    ), Symbol(row(arcLabelPos).toUpperCase))).toMap
    val arcLabels: Vector[Set[(Int, Symbol)]] = for {
      (neighborSet, i) <- neighbors.zipWithIndex
    } yield for {
      neighbor <- neighborSet
      if neighbor >= 0
    } yield (neighbor, arcLabelByTokenPair(Set(i, neighbor)))
    PolytreeParse(sentence, breadcrumb, children, arcLabels)
  }

  /** Returns an iterator that iterates over all of the words (as strings) that exist in
    * the argument parses, in order.
    *
    * Note that if a particular word appears multiple times in the parses, it will also appear
    * an equivalent number of times in the returned iterator.
    *
    * @param parses the parses we want to extract words from
    * @return an iterator over all words that appear in the argument parses (in order)
    */
  def extractWordsFromParses(parses: Iterator[PolytreeParse]): Iterator[String] = {
    for {
      parse: PolytreeParse <- parses
      token: Token <- parse.tokens
    } yield token.word.name
  }

  val arcInverterGoogleUniversal = new ArcInverter(Set('adp, 'adpmod, 'advmod, 'amod,
    'appos, 'aux, 'auxpass,
    'ccomp, 'compmod, 'dep, 'det, 'infmod, 'neg, 'nmod, 'num, 'p, 'parataxis, 'partmod,
    'poss, 'prt, 'rcmod, 'rel))

  val arcInverterStanford = new ArcInverter(Set('ADVCL, 'ADVMOD, 'AMOD, 'APPOS,
    'AUX, 'AUXPASS, 'CC, 'CONJ, 'DET, 'DISCOURSE, 'GOESWITH, 'MARK, 'MWE, 'NEG,
    'NPADVMOD, 'NN, 'NUM, 'NUMBER, 'POSS, 'POSSESSIVE, 'PRECONJ, 'PREDET,
    'PREP, 'PRT, 'PUNCT, 'QUANTMOD, 'RCMOD, 'TMOD, 'VMOD))

  implicit val jsFormat = jsonFormat4(PolytreeParse.apply)
}
