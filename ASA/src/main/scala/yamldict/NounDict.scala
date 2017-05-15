package yamldict
import fileIO._

/**
 * 名詞項構造データより辞書を構築
 */
class NounDict {

	def createDict(lines: Iterator[String]): NounDict = {
		lines.next
		val dict = this.createNounDict(lines)
		return dict
	}

	private def createNounDict(lines: Iterator[String]): NounDict = {
		val dict = new NounDict
		lines.foreach { line =>
			val nouns = new CsvReader().parseLine(line).get
			val noun = nouns(1) //見出し語 
			dict.frames.find(frame => frame.head.equals(noun)) match {
				case Some(frame) => frame.instances = frame.instances :+ this.createNounInstance(nouns)
				case None => dict.frames = dict.frames :+ this.createNounFrame(nouns)
			}
		}
		return dict
	}

	
	private def createNounFrame(nouns: List[String]): NounFrame = {
		val frame = new NounFrame
		frame.head = nouns(1)
		frame.support = nouns(3)
		frame.instances = frame.instances :+ this.createNounInstance(nouns)
		return frame
	}

	//Instanceクラス(事例をまとめたクラス)の作成
	private def createNounInstance(nouns: List[String]): NounInstance = {
		val instance = new NounInstance
		//Caseクラスの作成
		List(4, 8, 12).foreach { n =>
			nouns(n + 2).nonEmpty | nouns(n + 3).nonEmpty match {
				case true =>
					val ncase = new NounCase
					ncase.noun = nouns(n + 3)
					ncase.part = nouns(n + 2)
					ncase.arg = nouns(n + 1)
					ncase.semrole = nouns(n)
					instance.cases = instance.cases :+ ncase
				case false =>
			}
		}
		//Agentクラスの作成
		nouns(20).nonEmpty | nouns(22).nonEmpty match {
			case true =>
				val agent = new NounAgent
				agent.agentive = nouns(20) //AgentiveL
				agent.semantic = nouns(21) //AgentSem
				List(22, 23, 24).foreach { n => //AgentArg1~3
					nouns(n).nonEmpty match {
						case true =>
							val role = nouns(n).split("=")
							role(1) match {
								case "arg0" => agent.arg0 = role(0)
								case "arg1" => agent.arg1 = role(0)
								case "arg2" => agent.arg2 = role(0)
								case _ =>
							}
						case false =>
					}
				}
				instance.agents = instance.agents :+ agent
			case false =>
		}
		return instance
	}

	class NounDict {
		var frames: List[NounFrame] = List.empty
	}

	class NounFrame {
		var head: String = ""
		var support: String = ""
		var instances: List[NounInstance] = List.empty
	}

	class NounInstance {
		var cases: List[NounCase] = List.empty
		var agents: List[NounAgent] = List.empty
	}

	class NounCase {
		var noun: String = ""
		var part: String = ""
		var category: String = ""
		var semrole: String = ""
		var arg: String = ""
	}

	class NounAgent {
		var agentive: String = ""
		var semantic: String = ""
		var arg0: String = ""
		var arg1: String = ""
		var arg2: String = ""
	}
}