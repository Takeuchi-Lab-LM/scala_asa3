package toyaml
import fileIO._

class ToCchartYaml(xml: scala.xml.Elem, file: OutputFiles) {
	val tab = " " * 2
	def convert() {
		file.write("---")
		file.write("dict:")
		xml.\("cchart").foreach(convertCchart(_))
	}

	def convertCchart(cchart: scala.xml.Node) {
		val indent = tab * 0
		file.write(indent + "- " + "ctype: " + cchart.\("type").text)
		file.write(indent + tab + "form:")
		cchart.\("form").foreach(convertForms(_))
	}
	def convertForms(form: scala.xml.Node) {
		val indent = tab * 1
		file.write(indent + "- " + form.text)
	}
}