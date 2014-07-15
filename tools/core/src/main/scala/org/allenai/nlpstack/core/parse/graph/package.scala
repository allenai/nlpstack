package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.graph.Graph.Edge

package object graph {
  type Dependency = Edge[DependencyNode]
}
