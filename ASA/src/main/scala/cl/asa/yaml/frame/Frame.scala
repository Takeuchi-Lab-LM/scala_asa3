package cl.asa.yaml.frame

class Frame {
	var verb = ""
	var frame:Array[Semantic] = Array.empty
	
	def setVerb(str:String){
		this.verb = str
	}
	def setFrame(list:Array[Semantic]){
		this.frame = list
	}
}