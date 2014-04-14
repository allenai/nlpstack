package org.allenai.aitk.parse

import org.allenai.aitk.graph.Graph
import org.allenai.aitk.graph.Graph.Edge

package object graph {
  type Dependency = Edge[DependencyNode]
}