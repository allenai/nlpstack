package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.Format
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.core.postag.{ PostaggedToken, Postagger }
import org.allenai.nlpstack.core.tokenize.{ Token, Tokenizer }

/** A trait for a tool that produces a dependency graph, such as the Stanford dependency parser.
  * Subclasses should override dependencyGraphPostagged.
  */
trait DependencyParser {

  /** Create a dependency graph from POS-tagged tokens.
    * @param tokens the tokens to parse
    * @return the dependency graph built from the given tokens
    */
  def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph

  /** Create a dependency graph using a given postagger, for a given tokenized string.
    * @param postagger the postagger to use to POS tag the input tokens
    * @param tokens the tokenized string to parse
    * @return a tuple with the resultant POS tagged tokens and dependency graph
    */
  def dependencyGraphTokenized(postagger: Postagger)(tokens: Seq[Token]): (Seq[PostaggedToken], DependencyGraph) = {
    val postaggedTokens = postagger.postagTokenized(tokens)
    (postaggedTokens, dependencyGraphPostagged(postaggedTokens))
  }

  /** Create a dependency graph using a given tokenizer and postagger, for a given string.
    * @param tokenizer the tokenizer to use to parse the input string
    * @param postagger the postagger to use to POS tag the input string
    * @param string the string to parse
    * @return a tuple with the resultant POS tagged tokens and dependency graph
    */
  def dependencyGraph(tokenizer: Tokenizer, postagger: Postagger)(string: String): (Seq[PostaggedToken], DependencyGraph) = {
    val postaggedTokens = postagger.postag(tokenizer)(string)
    (postaggedTokens, dependencyGraphPostagged(postaggedTokens))
  }
}

object DependencyParser {
  object multilineStringFormat extends Format[(Seq[PostaggedToken], DependencyGraph), String] {
    def write(dgraph: (Seq[PostaggedToken], DependencyGraph)) = {
      val (tokens, graph) = dgraph
      val tokensPickled = Postagger.multilineStringFormat.write(tokens)
      val graphPickled = DependencyGraph.multilineStringFormat.write(graph)

      tokensPickled + "\n\n" + graphPickled
    }

    def read(pickled: String): (Seq[PostaggedToken], DependencyGraph) = {
      val (postagsPickled, depsPickled) = pickled.split("\n\n") match {
        case Array(postagsPickled, depsPickled) => (postagsPickled, depsPickled)
        case _ => throw new MatchError("Could not split pickled dgraph: " + pickled)
      }

      val postags = Postagger.multilineStringFormat.read(postagsPickled)
      val graph = DependencyGraph.multilineStringFormat.read(depsPickled)

      (postags, graph)
    }
  }
}
