package org.allenai.nlpstack

package object parse {
  val defaultDependencyParser: DependencyParser = new FactorieParser
}
