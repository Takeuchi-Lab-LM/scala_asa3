package toyaml
import fileIO._

class ToIdiomYaml(xml: scala.xml.Elem, file: OutputFiles) {
	val tab = " " * 2
	def convert() {
		file.write("---")
		file.write("dict:")
		xml.\("idiom").foreach(convertIdiom(_))
	}

	def convertIdiom(idiom: scala.xml.Node) {
		val indent = tab * 0
		file.write(indent + "- " + "entry: " + idiom.\("entry").text)
		idiom.\("paraphrase") match {
			case para if para.nonEmpty =>
				file.write(indent + tab + "phrase:")
				para.foreach(convertPhrase(_))
			case _ => None
		}
		file.write(indent + tab + "patterns:")
		idiom.\("pattern").foreach(convertPattern(_))
	}
	def convertPhrase(phrase: scala.xml.Node) {
		val indent = tab * 1
		file.write(indent + "- " + phrase.text)
	}

	def convertPattern(pattern: scala.xml.Node) {
		val matchs = """morph\[(.+)\]""".r
		val cases = pattern.text.split("\t").map { icase =>
			matchs.findFirstMatchIn(icase).get.group(1)
		}
		cases.reverse.foreach(convertCase(_))
	}

	def convertCase(icase: String) {
		val indent = tab * 1
		val matchs = """\((.+)\)""".r
		file.write(indent + "- " + "cases: ")
		icase.split(" or ").foreach { elem =>
			elem match {
				case elem if elem.contains("and") =>
					val elems = matchs.findFirstMatchIn(elem).get.group(1).split(" and ")
					convertElem(indent + tab + "- ", elems.head)
					convertElem(indent + tab * 2, elems.last)
				case elem =>
					convertElem(indent + tab + "- ", elem)
			}
		}
	}

	def convertElem(tab: String, elem: String) {
		val matchs = """(.+)=\"(.+)\"""".r
		val regex = matchs.findFirstMatchIn(elem)
		regex.get.group(1) match {
			case "base" => file.write(tab + "base: " + regex.get.group(2))
			case "base_read" => file.write(tab + "read: " + regex.get.group(2))
			case "pos" => file.write(tab + "pos: " + regex.get.group(2).replace('-', ','))
			case _ =>
		}
	}
}
