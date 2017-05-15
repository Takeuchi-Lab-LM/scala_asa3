package cl.asa.yaml.compoundPredicate

class CompoundPredicate {
	var entry: String = ""
	var phrase:Array[String] = Array.empty
	var patterns:Array[Pattern] = Array.empty
	var semantic: String = ""
		
	def setEntry(str:String){
		this.entry = str
	}
	
	def setPhrase(list:Array[String]){
		this.phrase = list
	}
	
	def setPatterns(list:Array[Pattern]){
		this.patterns = list
	}
	
	def setSemantic(str:String){
		this.semantic = str
	}
}