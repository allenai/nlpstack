package org.allenai.nlpstack.core.parse.graph

import org.allenai.nlpstack.core.graph.Graph.Edge
import org.allenai.nlpstack.core.graph.{ Direction, DownEdge, Graph, UpEdge }
import org.allenai.nlpstack.core._

import org.slf4j.LoggerFactory
import spray.json._

import scala.Option.option2Iterable
import scala.collection.immutable

/** A representation of a graph over dependencies.
  * This richer representation may include the text of the original sentence,
  * the original nodes (before collapsing), and the original dependencies.
  */
class DependencyGraph private (val root: Option[DependencyNode], vertices: Set[DependencyNode], edges: Set[Edge[DependencyNode]])
    extends Graph[DependencyNode](vertices, edges) {

  val nodes = vertices
  val dependencies = edges

  override def toString = DependencyGraph.singlelineStringFormat.write(this)

  /** Approximate Stanford's procedure to create collapsed dependencies. */
  def collapse = {
    /** Turn prepositions into edges instead of nodes. */
    def edgifyPrepositions(graph: Graph[DependencyNode]): Graph[DependencyNode] = {
      var g = graph

      // rename prep edges
      g = new Graph[DependencyNode](g.vertices, g.edges.map { e =>
        e.label match {
          case "prep" | "prepc" =>
            val qualifier = if (graph.dedges(e.dest) exists { case DownEdge(e) => e.label == "pcomp" case _ => false }) "c" else ""
            e.copy(label = e.label + qualifier + "_" + e.dest.string.toLowerCase.replaceAll(" ", "_"))
          case _ => e
        }
      })

      // NOTE: conjunctions must be distributed before pobj edges
      // are collapsed.  Otherwise some won't have incoming prep
      // edges to their targets yet.

      // collapse edges (pobj) preceeded by prep
      try {
        g = g.collapse { edge =>
          edge.label == "pobj" && (g.incoming(edge.source) exists (_.label startsWith "prep"))
        }((nodes: Traversable[DependencyNode]) =>
          nodes.find(n => g.edges(n).exists(e => e.label == "pobj" && e.dest == n)).get)
      } catch {
        case e: Throwable => DependencyGraph.logger.error("could not collapse pobj.", e)
      }

      // collapse edges (pcomp) preceeded by prep
      try {
        g = g.collapse { edge =>
          edge.label == "pcomp" && (g.incoming(edge.source) exists (_.label startsWith "prep"))
        }((nodes: Traversable[DependencyNode]) => {
          nodes.find(n => g.edges(n).exists(e => e.label == "pcomp" && e.dest == n)).get
        })
      } catch {
        case e: Throwable => DependencyGraph.logger.error("could not collapse pcomp.", e)
      }

      g
    }

    /** Collapse multi-word prepositions into a single node.  This will make illegal nodes. */
    def collapseMultiwordPrepositions(graph: Graph[DependencyNode]): Graph[DependencyNode] = {
      val preps = graph.edges.filter(edge => edge.label == "prep" || edge.label == "pcomp").toList.sortBy(_.dest.id)(Ordering[Int].reverse)

      // follow up prep, advmod, dep, amod edges
      def cond(e: Graph.Edge[DependencyNode]) = e.label == "prep" || e.label == "advmod" || e.label == "dep" || e.label == "amod"

      preps.foldLeft(graph) {
        case (graph, prep) =>
          if (!(graph.edges contains prep)) graph else {
            val last = prep.dest
            val predecessors = graph.vertices.filter(_.id <= last.id).toList.sortBy(_.id)(Ordering[Int].reverse)

            DependencyGraph.reversedSplitMultiwordPrepositions.filter(p => predecessors.map(_.string).startsWith(p)).toSeq match {
              case Seq() => graph
              case mtches =>
                val removeVertices = predecessors.take(mtches.maxBy(_.length).length).drop(1).flatMap(graph.inferiors(_, _.dest != last)).toSet.toList.sorted
                val joinVertices = removeVertices :+ last

                // keep last connected in case we remove some
                // of it's parents
                var parent = last
                while ((joinVertices contains parent) && (graph.indegree(parent) == 1)) {
                  parent = graph.incoming(parent).head.source
                }

                if (joinVertices contains parent) {
                  // we removed parents up to the root--abort
                  graph
                } else {
                  // add an edge from the closest remaining parent
                  // to last, if we need to
                  val extraEdges =
                    if (graph.neighbors(last) contains parent) Nil
                    else List(new Graph.Edge[DependencyNode](parent, last, "prep"))

                  val text = joinVertices.iterator.map(_.string).mkString(" ")
                  new Graph[DependencyNode](
                    extraEdges ++ graph.edges.filterNot(_.vertices exists (removeVertices contains _))).map(vertex =>
                    if (vertex == prep.dest) new DependencyNode(-1, text) // these nodes are only temporary
                    else vertex)
                }
            }
          }
      }
    }

    /** Turn junctions (and / or) into edges instead of nodes. */
    def collapseJunctions(graph: Graph[DependencyNode]) = {
      val conjGraph = graph.edges.filter(edge =>
        // conj edges to a node with no children
        edge.label == "conj" &&
          // source of conj edges has a child cc edge
          graph.dedges(edge.source).exists { case DownEdge(e) => e.label == "cc" case _ => false }).foldLeft(graph) {
        case (graph, conj) =>
          val ccNodes = graph.dedges(conj.source).filter {
            case DownEdge(e) => e.label == "cc"
            case _ => false
          }.iterator.map(_.edge.dest).toList

          // look left (negative distance) and then right.
          val bestCC = ccNodes.minBy {
            case cc =>
              val dist = math.abs(cc.id - conj.dest.id)
              if (dist < 0) -ccNodes.length - dist
              else dist
          }

          val newEdges = scala.collection.Set[Edge[DependencyNode]]() ++ graph.edges - conj + conj.copy(label = "conj_" + bestCC.string)

          new Graph[DependencyNode](graph.vertices, newEdges)
      }

      new Graph[DependencyNode](conjGraph.edges filterNot (_.label == "cc"))
    }

    /** Distribute some edges to other nodes connected by conj_and.
      *
      * Incoming/outgoing are defined as a direction relative to the
      * connected component joined by the conjunction.
      *
      * 1.  Distribute nsubj.
      * a.  "Michael, Rob, and NJ went to Than Vi."
      * b.  "The apple was crisp and fresh."
      * 2.  Distribute nsubjpass.
      * a.  incoming: "The bullet and gunpowder was loaded and fired."
      * b.  outgoing: "The bullet was loaded and fired."
      * 3.  Distribute incoming advmod edges
      * a.  incoming: "He spoke wisely and surely."
      * b.  outgoing: "Just write them down and I will edit it for you."
      * 4.  Distribute incoming acomp edges
      * a.  incoming: "The water looked blue and refreshing.
      * 5.  Distribute incoming amod edges
      * a.  incoming: "The blue and cool water felt nice."
      * b.  outgoing: "Pills raise clotting , high blood pressure , heart attack , and stroke . "
      * 6.  Distribute incoming dobj edges
      * a.  incoming: "Michael found rocks and spiders."
      * b.  outgoing: "Michael went to the beach and found rocks."
      * 7.  Distribute incoming rcmod edges
      * a.  incoming: "The woman, who wore a black dress and spoke in the theater, ate cheese."
      * b.  outgoing:
      * 8.  Distribute incoming ccomp edges
      * a.  incoming: "He says you swim fast and eat cherries."
      * 9.  Distribute incoming xcomp edges
      * a.  incoming: "He says you like to swim fast and eat cherries."
      * 10. Distribute incoming prep edges
      * a.  incoming: "Michael and George went to the beach in Spring and Fall."
      * b.  outgoing: "Michael and George went to the beach and slept."
      */
    def distributeConjunctions(graph: Graph[DependencyNode]) = {
      // find components connected by conj_and
      val components = graph.components(e => (e.label equalsIgnoreCase "conj_and") || e.label == "conj_&")

      val newEdges = components.flatMap { vertices =>
        val dedges = vertices.flatMap(graph.dedges(_))

        // find new edges needed to distribute conjunction
        for (
          dedge <- dedges;
          if (dedge.edge.label == "nsubj" ||
            dedge.edge.label == "nsubjpass" ||
            dedge.dir == Direction.Down && (
              // distribute "to" in: "I want to swim and eat cherries"
              dedge.edge.label == "aux") ||
              dedge.dir == Direction.Up && (
                dedge.edge.label == "advmod" ||
                dedge.edge.label == "amod" ||
                dedge.edge.label == "acomp" ||
                dedge.edge.label == "dobj" ||
                dedge.edge.label == "rcmod" ||
                dedge.edge.label == "ccomp" ||
                dedge.edge.label == "xcomp" ||
                (dedge.edge.label startsWith "prep")));
          if !(vertices contains dedge.end);
          v <- vertices;
          newEdge = dedge match {
            case DownEdge(e) => e.copy(source = v)
            case UpEdge(e) => e.copy(dest = v)
          };
          if !(newEdge.source == newEdge.dest);
          if !(graph.edges contains newEdge)
        ) yield (newEdge)
      }

      new Graph[DependencyNode](graph.vertices, graph.edges ++ newEdges)
    }

    /** Runs a graph transformation and returns the result.
      *
      * After the transformation, it checks whether we still have a single
      * root. If not, it returns the original, untransformed graph.
      */
    def runStep(
      transformation: Graph[DependencyNode] => Graph[DependencyNode],
      graph: Graph[DependencyNode]) = {
      // Uncomment for debugging
      //println(graph.toDot())

      val newGraph = transformation(graph)

      // if this transformation produced two root nodes, ignore the
      // transformation
      if (newGraph.isTree)
        newGraph
      else
        graph
    }

    /** Removes nodes with a temporary id
      *
      * These nodes are sometimes left over from other processing steps.
      */
    def removeInvalidNodes(graph: Graph[DependencyNode]) =
      new Graph[DependencyNode](
        graph.vertices.filter(_.id >= 0),
        graph.edges.filter(e => e.source.id >= 0 && e.dest.id >= 0))

    val graph =
      runStep(removeInvalidNodes, (
        runStep(edgifyPrepositions, (
          runStep(distributeConjunctions, (
            runStep(collapseJunctions, (
              runStep(collapseMultiwordPrepositions, this)))))))))

    DependencyGraph(graph.vertices, graph.edges)
  }

  /** Simplify xsubj and nsubj to just subj. */
  def collapseXNsubj = {
    val edges = this.edges.map { dep =>
      if ((dep.label equals "xsubj") || (dep.label equals "nsubj"))
        new Edge[DependencyNode](dep.source, dep.dest, "subj")
      else dep
    }
    DependencyGraph(edges)
  }

  def joined: JoinedDependencyGraph = {
    val joinedNodes = this.vertices map JoinedDependencyNode.from
    val joinedEdges = this.edges map { edge =>
      edge.copy(
        source = JoinedDependencyNode.from(edge.source),
        dest = JoinedDependencyNode.from(edge.dest))
    }
    new JoinedDependencyGraph(joinedNodes, joinedEdges)
  }

  def tokenized(tokens: Seq[Lemmatized[PostaggedToken]]): Graph[TokenDependencyNode] = {
    def from = TokenDependencyNode.from(tokens) _
    val joinedNodes = this.vertices map from
    val joinedEdges = this.edges map { edge =>
      edge.copy(
        source = from(edge.source),
        dest = from(edge.dest))
    }
    new Graph[TokenDependencyNode](joinedNodes, joinedEdges)
  }
}

object DependencyGraph {
  val logger = LoggerFactory.getLogger(this.getClass)

  type JoinedDependencyGraph = Graph[JoinedDependencyNode]

  def apply(root: Option[DependencyNode], vertices: Set[DependencyNode], edges: Set[Edge[DependencyNode]]): DependencyGraph = {
    import org.allenai.nlpstack.core.parse.graph.Dependency.DependencyOrdering

    val sortedVertices = immutable.SortedSet.empty[DependencyNode] ++ vertices
    val sortedEdges = immutable.SortedSet.empty[Dependency] ++ edges

    new DependencyGraph(root, sortedVertices, sortedEdges)
  }

  def apply(vertices: Set[DependencyNode], edges: Set[Edge[DependencyNode]]): DependencyGraph = {
    val roots = vertices.filter { v =>
      // There's no edge that ends at the root.
      edges forall (e => e.dest != v)
    }

    val root: Option[DependencyNode] = {
      if (roots.isEmpty && !vertices.isEmpty)
        throw new IllegalArgumentException("There must be a root: " + vertices)
      else if (roots.size > 1)
        throw new IllegalArgumentException("There must be a single root: " + roots)
      else
        roots.headOption
    }

    this.apply(root, vertices, edges)
  }

  def apply(dependencies: Iterable[Dependency]): DependencyGraph = {
    this.apply(dependencies.flatMap(_.vertices).toSet, dependencies.toSet)
  }

  def create(dependencies: Iterable[Dependency]): DependencyGraph = {
    this.apply(dependencies)
  }

  implicit object dependencyJsonFormat extends RootJsonFormat[Edge[DependencyNode]] {
    def write(edge: Edge[DependencyNode]): JsValue = {
      JsObject(
        "label" -> JsString(edge.label),
        "source" -> edge.source.toJson,
        "dest" -> edge.dest.toJson)
    }

    def read(value: JsValue): Edge[DependencyNode] = {
      value.asJsObject.getFields("label", "source", "dest") match {
        case Seq(JsString(label), sourceJsObject, destJsObject) =>
          val source = sourceJsObject.convertTo[DependencyNode]
          val dest = destJsObject.convertTo[DependencyNode]
          new Edge[DependencyNode](source, dest, label)
        case _ => deserializationError("Dependency expected.")
      }
    }
  }

  implicit object dependencyGraphJsonFormat extends RootJsonFormat[DependencyGraph] {
    def write(graph: DependencyGraph): JsValue = {
      JsArray(graph.edges.toList map (_.toJson))
    }

    def read(value: JsValue): DependencyGraph = value match {
      case JsArray(edgeJsObjects) =>
        val edges = edgeJsObjects map (_.convertTo[Edge[DependencyNode]])
        DependencyGraph.apply(edges)
      case _ => deserializationError("Dependencies expected.")
    }
  }

  object singlelineStringFormat extends StringFormat("; ")
  object multilineStringFormat extends StringFormat("\n")

  class StringFormat(seperator: String) extends Format[DependencyGraph, String] {
    def write(graph: DependencyGraph) = {
      import org.allenai.nlpstack.core.parse.graph.Dependency.DependencyOrdering

      // create a root dependency
      val root = graph.root.map { root =>
        new Edge[DependencyNode](
          new DependencyNode(0, "ROOT"),
          root.copy(id = root.id + 1),
          "root")
      }

      // increment all dependency node ids
      val incrementedDeps: immutable.SortedSet[Dependency] = immutable.SortedSet[Dependency]() ++ root ++
        graph.dependencies.map { dep =>
          dep.copy(
            source = dep.source.copy(id = dep.source.id + 1),
            dest = dep.dest.copy(id = dep.dest.id + 1))
        }

      // serialize dependencies on first line
      val pickledDeps = incrementedDeps.iterator map Dependency.stringFormat.write

      // create a pickled string
      pickledDeps.mkString(seperator)
    }

    def read(pickled: String) = {
      import org.allenai.nlpstack.core.parse.graph.Dependency.DependencyOrdering

      // Split dependencies (edges).
      val pickledDeps = pickled.split(seperator)

      // Deserialize dependencies.
      val allDeps = pickledDeps map Dependency.stringFormat.read

      // Separate the root and decrement node ids so they are zero indexed.
      val rootDep = allDeps.find(_.label == "root")

      val deps: immutable.SortedSet[Dependency] =
        (allDeps filterNot (dep => rootDep exists (_ == dep))).map(dep =>
          dep.copy(
            source = dep.source.copy(id = dep.source.id - 1),
            dest = dep.dest.copy(id = dep.dest.id - 1)))(scala.collection.breakOut)

      val root = rootDep map (rootDep => rootDep.dest.copy(id = rootDep.dest.id - 1))

      val vertices = (deps flatMap (_.vertices)).toSet
      DependencyGraph(root, vertices, deps)
    }
  }

  class SerializationException(message: String, cause: Throwable)
    extends RuntimeException(message, cause)

  val reversedSplitMultiwordPrepositions = Postagger.complexPrepositions.map(_.split(" ").toList.reverse)
}
