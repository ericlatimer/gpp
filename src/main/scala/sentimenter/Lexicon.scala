package sentimenter

import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,Solver}
import nak.util.ConfusionMatrix
import chalk.lang.eng.Twokenize

/**
 * 
 * Implementation of Lexicon ratio baseline 
 * 
 * 
 */
object Lexicon {

  def apply(evalfile:String,details:Boolean) {
    //Digest eval data
    val evalXML = scala.xml.XML.loadFile(evalfile)
    val allEvalLabels = (evalXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val allEvalTweets = (evalXML \\ "content").map{x => x.text}
    val allEvalPairs = allEvalLabels.zip(allEvalTweets).filter(x=>List("positive","negative","neutral").contains(x._1))

    def readNative(pairs: Seq[(String,String)]) = 
      for (pair <- pairs)
        yield Example(pair._1, pair._2)
   
    // Make predictions on the evaluation data. 
    val nativeEvalExamples = readNative(allEvalPairs).toList
    val comparisons = for (ex <- nativeEvalExamples) yield {
      (ex.label, getPolarity(ex.features))
    }

    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions) = comparisons.unzip
    val inputs = nativeEvalExamples.map(_.features)
    //println("inputs: " + inputs.head)
    val cm = ConfusionMatrix(goldLabels, predictions, inputs)
    println(cm)
    if (details)
      println(cm.detailedOutput)
  }

  val polarityCheck = new WordLists


  // Input: Sentence/tweet text
  // Output: overall polarity of text (1 -> positive, -1 -> negative, 0 -> neutral
  // uses lists of positive and negative words from Project Phase 2
  def getPolarity(tweet: String) = {
    val sum = Twokenize(tweet).map{word => 
      if (polarityCheck.posWords.contains(word.toLowerCase)) 1 
        else if (polarityCheck.negWords.contains(word.toLowerCase)) -1
        else 0
    }.sum
    if (sum > 0) "positive" else if (sum < 0) "negative" else "neutral"
  }
}