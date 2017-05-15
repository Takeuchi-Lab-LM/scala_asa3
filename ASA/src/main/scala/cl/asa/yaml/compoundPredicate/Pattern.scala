package cl.asa.yaml.compoundPredicate

class Pattern {
	var entry: String = ""
	var cases:Array[Case] = Array.empty
	
	def setEntry(str:String){
		this.entry = str
	}
	
	def setCases(list:Array[Case]){
		this.cases = list
	}
}