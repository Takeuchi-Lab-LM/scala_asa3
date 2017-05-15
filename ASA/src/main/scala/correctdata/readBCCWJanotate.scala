package correctdata
import scala.io.Source

class readBCCWJanotate {
	def bccwj(file: String): List[List[List[String]]] = {
		val tsvs = Source.fromFile(file).getLines
		var instances: List[List[List[String]]] = List.empty
		var tsv: String = ""
		tsvs.next
		for (id <- tsvs) {
			var instance = List.empty[List[String]]
			while ({ tsv = tsvs.next; !tsv.equals("EOS") }) {
				instance = instance :+ tsv.split("\t").toList.padTo(6, "")
			}
			instances = instances :+ instance
		}
		return instances
	}
}