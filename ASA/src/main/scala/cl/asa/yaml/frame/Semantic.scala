package cl.asa.yaml.frame

class Semantic {
	var semantic = ""
	var instance:Array[Instance] = Array.empty

	def setSemantic(str:String){
		this.semantic = str
	}
	
	def setInstance(list:Array[Instance]){
		this.instance = list
	}
}