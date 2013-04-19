package sentimenter

import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,Solver}
import nak.util.ConfusionMatrix
/**
 * run-main example.TweetExample data/debate08/train.xml data/debate08/dev.xml
 * /Users/eric/Dropbox/CS395T-ANLP/gpp/data/debate08/train.xml
 * /Users/eric/Dropbox/CS395T-ANLP/gpp/data/debate08/dev.xml
 * @author jasonbaldridge
 */
object Majority {

  def apply(trainfile:String,evalfile:String) {
    //Digest training data
    val trainXML = scala.xml.XML.loadFile(trainfile)
    val trueTrainingLabels = (trainXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val majorityLabels = trueTrainingLabels.groupBy(x => x).map{pair => (pair._1, pair._2.length)}
    val flippedMajority = majorityLabels map {_.swap}
    val majority = flippedMajority.toList.sortBy(-_._1).head._2

    val allTrainingTweets = (trainXML \\ "content").map{x => x.text}
    val allTrainingPairs = trueTrainingLabels.zip(allTrainingTweets)

    val filteredTrainingPairs = allTrainingPairs.filter(x=>List("positive","negative","neutral").contains(x._1))
    val majorityTrainingPairs = filteredTrainingPairs.map(x => (majority,x._2))
    
    //Digest eval data
    val evalXML = scala.xml.XML.loadFile(evalfile)
    val allEvalLabels = (evalXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val allEvalTweets = (evalXML \\ "content").map{x => x.text}
    val allEvalPairs = allEvalLabels.zip(allEvalTweets).filter(x=>List("positive","negative","neutral").contains(x._1))

    def readNative(traininPairs: Seq[(String,String)]) = 
      for (pair <- traininPairs)
        yield Example(pair._1, pair._2)

    // Get the training examples in their native format.  
    val nativeExamples = readNative(majorityTrainingPairs).toList
    //println("Native example: " + nativeExamples.head)

    val featurizer = new Featurizer[String,String] {
	def apply(input: String) = {
        val words = input.split("\\s")
        for ((a,l) <- words zip Stream.continually(majority)) 
          yield FeatureObservation(l+"="+a)
      }
    }

    // Make predictions on the evaluation data. 
    val nativeEvalExamples = readNative(allEvalPairs).toList
    val comparisons = for (ex <- nativeEvalExamples) yield {
        // Output both the true label and the predicted label.
        (ex.label, majority,ex.features)   
    }

    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions,inputs) = comparisons.unzip3
    val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
    println(cmatrix)
  }
}