package toyaml
import fileIO._

class ToFrameYaml(xml: scala.xml.Elem, file: OutputFiles) {
	val tab = " " * 2
	def convert() {
		file.write("---")
		file.write("dict:")
		xml.\("frames").foreach(convertFrame(_))
	}
	def convertFrame(frame: scala.xml.Node) {
		val indent = tab * 0
		file.write(indent + "- " + "verb: " + frame.\("verb").text)
		file.write(indent + tab + "frame:")
		frame.\("frame").foreach(convertSemantic(_))
	}

	def convertSemantic(semantic: scala.xml.Node) {
		val indent = tab * 1
		file.write(indent + "- " + "semantic: " + semantic.\("semantic").text)
		file.write(indent + tab + "instance:")
		semantic.\("instance") foreach (convertInstance(_))
	}

	def convertInstance(instance: scala.xml.Node) {
		val indent = tab * 2
		instance.\("case") match {
			case cases if cases.nonEmpty =>
				file.write(indent + "- " + "cases:")
				cases.foreach(convertCase(_))
			case _ =>
		}
	}

	def convertCase(icase: scala.xml.Node) {
		val indent = tab * 3
		file.write(indent + "- " + "part: " + icase.\("the_part").text)
		icase.\("noun").text match {
			case noun if noun.nonEmpty => file.write(indent + tab + "noun: " + noun)
			case _ =>
		}
		icase.\("category").text match {
			case category if category.nonEmpty => file.write(indent + tab + "category: " + category)
			case _ =>
		}
		file.write(indent + tab + "semrole: " + icase.\("semrole").text)
		file.write(indent + tab + "weight: " + icase.\\("weight").text.toFloat)
	}

}