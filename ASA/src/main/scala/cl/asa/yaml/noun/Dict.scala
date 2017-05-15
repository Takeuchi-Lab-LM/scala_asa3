package cl.asa.yaml.noun

class Dict {
	var dict: Array[Frame] = Array.empty

	def setDict(list: Array[Frame]) {
		this.dict = list
	}

	def isFrame(noun: String): Boolean = {
		val bool: Boolean = noun.nonEmpty match {
			case true =>
				dict.exists { frame =>
					frame.head.equals(noun) | (frame.head + frame.support).equals(noun) |
						frame.head.equals(noun.init) | (frame.head + frame.support).equals(noun.init)
				}
			case false => false
		}
		return bool
	}

	def getFrame(noun: String): Option[Frame] = {
		val frame: Option[Frame] = noun.nonEmpty match {
			case true =>
				dict.find { frame =>
					frame.head.equals(noun) | (frame.head + frame.support).equals(noun) |
						frame.head.equals(noun.init) | (frame.head + frame.support).equals(noun.init)
				}
			case false => None
		}
		return frame
	}
}