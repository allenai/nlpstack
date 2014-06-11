package org.allenai.nlpstack.parse

import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.graph.Graph.Edge

package object graph {
  type Dependency = Edge[DependencyNode]
}
