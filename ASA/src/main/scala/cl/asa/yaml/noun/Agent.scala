package cl.asa.yaml.noun

class Agent {
	var agentive: String = ""
	var semantic: String = ""
	var arg0: String = ""
	var arg1: String = ""
	var arg2: String = ""

	def setAgentive(str: String) {
		this.agentive = str
	}

	def setSemantic(str: String) {
		this.semantic = str
	}

	def setArg0(str: String) {
		this.arg0 = str
	}

	def setArg1(str: String) {
		this.arg1 = str
	}

	def setArg2(str: String) {
		this.arg2 = str
	}
}