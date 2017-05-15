package cl.asa.yaml.noun

class Instance {
	var cases:Array[Case] = Array.empty
	var agent:Array[Agent] = Array.empty
	
	def setCases(list:Array[Case]){
		this.cases = list
	}
	
	def setAgent(list:Array[Agent]){
		this.agent = list
	}
}