package cl.asa.yaml.idiom

class Idiom {
	var entry: String = ""
	var phrase:Array[String] = Array.empty
	var patterns:Array[Pattern] = Array.empty
	
	def setEntry(str:String){
		this.entry = str
	}
	def setPhrase(list:Array[String]){
		this.phrase = list
	} 
	def setPatterns(list:Array[Pattern]){
		this.patterns = list
	}
}