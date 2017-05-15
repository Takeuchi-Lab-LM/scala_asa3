package toyaml
import fileIO._

class ToCategoryYaml(xml: scala.xml.Elem, file: OutputFiles) {
	val tab = " " * 2
	def convert() {
		file.write("---")
		file.write("dict:")
		xml.\("category").foreach(convertCategory(_))
	}

	def convertCategory(category: scala.xml.Node) {
		val indent = tab * 0
		file.write(indent + "- " + "category_name: " + category.\("category_name").text)
		file.write(indent + tab + "noun: ")
		category.\("noun").foreach(convertNoun(_))
	}

	def convertNoun(noun: scala.xml.Node) {
		val indent = tab * 1
		file.write(indent + "- " + noun.text)
	}
}