package cl.asa.parse.semantic
import cl.asa.result._
import cl.asa.yaml.noun._
import cl.asa.yaml.frame._
import cl.asa.yaml._

class NounStructure(nouns: noun.Dict, frames: frame.Dict2) {

	type NounSet = (Option[(Chunk, noun.Case)], Option[(Chunk, noun.Case)], Option[(Chunk, noun.Case)], Option[noun.Agent])

	def parse(chunk: Chunk) {
		val comp = nouns.getFrame(chunk.main) match {
			case Some(frame) =>
				val nounset = frame.instance.map { instance =>
					val (similar, insts) = this.calculateSntSimilar(instance, chunk)
					(similar, insts, instance.agent.headOption)
				}.maxBy(_._1)
				this.setSemantic(chunk, nounset._3)
				this.setFrame(nounset)
			case None =>
		}
	}

	private def calculateSntSimilar(instance: noun.Instance, chunk: Chunk): (Float, Seq[(Float, noun.Case, Chunk)]) = {
		var comb: Seq[(Float, noun.Case, Chunk)] = this.calculateAllCombinations(instance, chunk)
		var insts: Seq[(Float, noun.Case, Chunk)] = Seq.empty
		while (comb.exists(_._1 > 0)) {
			val max = comb.maxBy(pairs => pairs._1)
			insts = insts :+ max
			comb = comb.filterNot { arg =>
				arg._2.eq(max._2) || arg._3.eq(max._3)
			}
		}

		val similar = insts.map(_._1).sum
		instance.agent
		return (similar, insts)
	}

	private def calculateAllCombinations(instance: noun.Instance, chunk: Chunk): Seq[(Float, noun.Case, Chunk)] = {
		val chunks = chunk.modifyingchunk.ne(null) match {
			case true => chunk.modifiedchunks :+ chunk.modifyingchunk
			case false => chunk.modifiedchunks
		}
		val combinations = chunks.flatMap { chunk =>
			instance.cases.map { icase =>
				val similar = this.calculateArgSimilar(icase, chunk)
				(similar, icase, chunk)
			}
		}
		return combinations
	}

	private def calculateArgSimilar(icase: noun.Case, chunk: Chunk): Float = {
		val partsimilar = this.getPartSimilar(icase, chunk)
		val surfsimilar = 0
		val nounsimilar = 0
		val similar = partsimilar + surfsimilar + nounsimilar
		return similar
	}

	private def getPartSimilar(icase: noun.Case, chunk: Chunk): Float = {
		val similar = icase.part match {
			case "だ" if chunk.ctype.equals("copula") => 1.0F
			case "だ" if chunk.part.equals("は") | chunk.part.equals("が") => 1.0F
			case part if chunk.part.equals(part) => 1.0F
			case _ => 0.0F
		}
		return similar
	}

	def setSemantic(chunk: Chunk, agent: Option[noun.Agent]) {
		agent match {
			case Some(age) =>
				chunk.noun_agentiveL = age.agentive
				chunk.noun_semantic = age.semantic
			case None =>
				chunk.noun_semantic = "Null/Null/Null"
		}
	}

	def setFrame(set: (Float, Seq[(Float, noun.Case, Chunk)], Option[Agent])) {
		val (similar, insts, agent) = set
		insts.foreach { pair =>
			val (argsimilar, icase, chunk) = pair
			chunk.noun_semrole = icase.semrole
			chunk.noun_arg = icase.arg
			agent match {
				case Some(age) if icase.arg.equals("ARG0") => chunk.noun_agentiveRole = age.arg0
				case Some(age) if icase.arg.equals("ARG1") => chunk.noun_agentiveRole = age.arg1
				case Some(age) if icase.arg.equals("ARG2") => chunk.noun_agentiveRole = age.arg2
				case None =>
			}
		}
	}
}