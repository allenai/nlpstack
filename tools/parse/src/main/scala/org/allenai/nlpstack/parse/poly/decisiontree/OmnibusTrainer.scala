package org.allenai.nlpstack.parse.poly.decisiontree

class OmnibusTrainer()
    extends ProbabilisticClassifierTrainer {

  val dtTrainer = new DecisionTreeTrainer(0.3, "entropy")
  val rfTrainer = new RandomForestTrainer(0, 10, 100, "entropy")

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

