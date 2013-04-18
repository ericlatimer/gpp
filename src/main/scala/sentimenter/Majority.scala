package sentimenter

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

    val allTrainingLabels = (trainXML \\ "item").map { item =>
      majority
    }

    val allTrainingTweets = (trainXML \\ "content").map{x => x.text}
    val allTrainingPairs = allTrainingLabels.zip(allTrainingTweets)

    //Digest eval data
    val evalXML = scala.xml.XML.loadFile(evalfile)
    val allEvalLabels = (evalXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val allEvalTweets = (evalXML \\ "content").map{x => x.text}
    val allEvalPairs = allEvalLabels.zip(allEvalTweets)

    def readNative(traininPairs: Seq[(String,String)]) = 
      for (pair <- traininPairs)
        yield Example(pair._1, pair._2)

    
    // Get the training examples in their native format.  
    val nativeExamples = readNative(allTrainingPairs).toList
    println("Native example: " + nativeExamples.head)

    // A featurizer that simply splits the native inputs and attaches the
    // appropriate attributes to each of the elements.
    val negativeFeaturizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val words = input.split("\\s")
        for ((a,l) <- words zip Stream.continually("negative")) 
          yield FeatureObservation(l+"="+a)
      }
    }
    val neutralFeaturizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val words = input.split("\\s")
        for ((a,l) <- words zip Stream.continually("neutral")) 
          yield FeatureObservation(l+"="+a)
      }
    }
    val positiveFeaturizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val words = input.split("\\s")
        for ((a,l) <- words zip Stream.continually("positive")) 
          yield FeatureObservation(l+"="+a)
      }
    }    
    
    // Now apply the featurizer to the examples. We are mapping over a sequence
    // of Examples, and for each Example, we map the featurizer over it. The latter
    // simply applies the featurizer to the 'features' value in the Example.
    //nativeExamples.map{ex => println(ex)}
    val featurizedExamples = nativeExamples.map(ex => ex.label match {
                              case "negative" => ex.map(negativeFeaturizer)
                              case "neutral" => ex.map(neutralFeaturizer)
                              case "positive" => ex.map(positiveFeaturizer)
                              }
                            )
    println("Featurized example: " + featurizedExamples.head)

    
    // The features were extracted above, but they are still represented as
    // Strings. Liblinear needs these to be numerical indices, and the 
    // ExampleIndexer will take care of this for us and give us the resulting
    // mapping from labels to indices and features to indices.
    val indexer = new ExampleIndexer
    val indexedExamples = featurizedExamples.map(indexer)
    val (lmap,fmap) = indexer.getMaps
    println("Indexed example: " + indexedExamples.head)


    // Configure and train with liblinear. Here we use the L2-Regularized 
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.
    val config = new LiblinearConfig(Solver("L2R_LR"), .5)
    val classifier = LiblinearTrainer.train(indexedExamples, lmap, fmap, config)

    // Make predictions on the evaluation data. 
    val nativeEvalExamples = readNative(allEvalPairs).toList
    val comparisons = for (ex <- nativeEvalExamples) yield {
      ex.label match {
        case "negative" => {
          val scores = classifier.evalUnindexed(negativeFeaturizer(ex.features))
          // Get the *index* of the best score.
          val best = scores.zipWithIndex.maxBy(_._1)._2

          // Output both the true label and the predicted label.
          (ex.label, classifier.labelOfIndex(best))
          }
        case "neutral" => {
          val scores = classifier.evalUnindexed(neutralFeaturizer(ex.features))
          // Get the *index* of the best score.
          val best = scores.zipWithIndex.maxBy(_._1)._2

          // Output both the true label and the predicted label.
          (ex.label, classifier.labelOfIndex(best))
          }
        case "positive" => {
          val scores = classifier.evalUnindexed(positiveFeaturizer(ex.features))
          // Get the *index* of the best score.
          val best = scores.zipWithIndex.maxBy(_._1)._2

          // Output both the true label and the predicted label.
          (ex.label, classifier.labelOfIndex(best))
          }
        }
    }

    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions) = comparisons.unzip
    val inputs = nativeEvalExamples.map(_.features)
    val cmatrix = ConfusionMatrix(goldLabels, predictions, inputs)
    println(cmatrix)
  }
}