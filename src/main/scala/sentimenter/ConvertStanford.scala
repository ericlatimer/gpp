package sentimenter
import scala.io.Source


object ConvertStanford {


	def main(args: Array[String]){
		val data = io.Source.fromFile(args(0)).getLines

		val dataset = <dataset>
{for (line <- data)yield { val information = line.split(";;")
				val polarity = if (information(0) == "0") "negative"
						else if (information(0) == "2") "neutral"
						else  "positive"    
<item tweetid={information(1).toString} label={polarity.toString} target={information(3).toString} username={information(4).toString}>
	<content>{information(5).toString}</content>
	</item>
}}
</dataset>

		println(dataset)
	}

}

