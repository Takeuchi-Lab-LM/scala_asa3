package cl.asa.yaml.cchart

class Cchart {
	var form: Array[String] = Array.empty
	var ctype: String = ""

	def setForm(list: Array[String]) {
		this.form = list
	}

	def setCtype(str: String) {
		this.ctype = str
	}
}