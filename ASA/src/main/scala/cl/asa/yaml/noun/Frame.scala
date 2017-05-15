package cl.asa.yaml.noun

class Frame {
	var head:String = ""
	var support:String = ""
	var instance:Array[Instance] = Array.empty
	
	def setHead(str:String) {
		this.head = str
	}
	
	def setSupport(str:String){
		this.support = str
	}
	
	def setInstance(list:Array[Instance]){
		this.instance = list
	}
}