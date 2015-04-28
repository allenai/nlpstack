package org.allenai.nlpstack.parse.poly.decisiontree

class OmnibusTrainer()
    extends ProbabilisticClassifierTrainer {

  val dtTrainer = new RandomForestTrainer(0, 12, 0.1f, MultinomialGainMetric(0.5f), numThreads = 6)
  val rfTrainer = new RandomForestTrainer(0, 12, 0.1f, MultinomialGainMetric(0.5f), numThreads = 6)

  override def apply(data: FeatureVectorSource): ProbabilisticClassifier = {
    val trainer = data.classificationTask.filenameFriendlyName match {
      case name if name.startsWith("dt-") =>
        dtTrainer
      case _ =>
        rfTrainer
    }
    trainer(data)
  }
}

