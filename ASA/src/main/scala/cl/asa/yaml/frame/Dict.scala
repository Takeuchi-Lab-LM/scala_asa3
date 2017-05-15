package cl.asa.yaml.frame

import scala.collection.JavaConversions._

class Dict {
	var dict: Array[Frame] = Array.empty

	def setDict(list: Array[Frame]) {
		this.dict = list
	}

	def getFrame(verb: String): Option[Frame] = {
		val frame = dict.find { frame =>
			frame.verb.equals(verb)
		}
		return frame
	}

	def isFrame(verb: String): Boolean = {
		val bool: Boolean = dict.exists { frame =>
			frame.verb.equals(verb)
		}
		return bool
	}
}