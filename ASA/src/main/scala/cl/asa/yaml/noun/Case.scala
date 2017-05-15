package cl.asa.yaml.noun

class Case {
	var noun:String = ""
	var part:String = ""
	var category:String = ""
	var semrole:String = ""
	var arg:String = ""
		
	def setNoun(str:String){
		this.noun = str
	}
	
	def setPart(str:String){
		this.part = str
	}
	
	def setCategory(str:String){
		this.category
	}
	
	def setSemrole(str:String){
		this.semrole = str
	}
	
	def setArg(str:String){
		this.arg = str
	}
}