package cl.asa.yaml.frame

class Instance {
	var cases:Array[Case] =Array.empty
	
	def setCases(list:Array[Case]){
		this.cases = list
	}
}