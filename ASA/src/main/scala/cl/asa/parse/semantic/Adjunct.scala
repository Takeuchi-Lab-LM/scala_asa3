package cl.asa.parse.semantic
/**
 * 追加詞を付与するためのクラス
 */
import cl.asa.result._

class Adjunct {
	def parse(modifiedlinks: Seq[Chunk]) {
		modifiedlinks.foreach { modchunk =>
			modchunk.adjunct = getAdjunct(modchunk)
			modchunk.semrole = modchunk.adjunct.nonEmpty match {
				case true if modchunk.semrole.isEmpty => Seq(modchunk.adjunct)
				case true if modchunk.similar > 2.0 => modchunk.semrole
				case true => Seq(modchunk.adjunct)
				case false => modchunk.semrole
			}
		}
	}

	private def getAdjunct(chunk: Chunk): String = {

		var adjunct: String = ""
		if (adjunct.isEmpty) { adjunct = parseTime(chunk) }
		if (adjunct.isEmpty) { adjunct = parseLocation(chunk) }
		if (adjunct.isEmpty) { adjunct = parseScene(chunk) }
		if (adjunct.isEmpty) { adjunct = parseInstrument(chunk) }
		//if (adjunct.isEmpty) { adjunct = parseAs(chunk) }追加詞の変更により削除
		//if (adjunct.isEmpty) { adjunct = parseAround(chunk) }追加詞の変更により削除
		if (adjunct.isEmpty) { adjunct = parseReason(chunk) }
		if (adjunct.isEmpty) { adjunct = parseLimit(chunk) }
		if (adjunct.isEmpty) { adjunct = parsePremise(chunk) } //未定義
		//if (adjunct.isEmpty) { adjunct = parseCitation(chunk) }追加詞の変更により削除
		if (adjunct.isEmpty) { adjunct = parsePurpose(chunk) }
		if (adjunct.isEmpty) { adjunct = parseModificand(chunk) } //未定義
		if (adjunct.isEmpty) { adjunct = parseManner(chunk) } //未定義
		return adjunct
	}

	private def parseTime(chunk: Chunk): String = {
		val time: String = chunk.category.headOption match {
			case None => ""
			case Some(head) => head match {
				case "時間" =>
					chunk.morphs.exists(_.surface.contains("間")) match {
						case true => "場所（時）（間）" //"Time-Line"
						case false => "場所（時）（点）" //"Time-point"
					}
				case "動作" =>
					chunk.morphs match {
						case morphs if morphs.exists(_.surface.contains("間")) =>
							"場所（時）（間）" //"Time-Line"
						case morphs if morphs.exists { morph =>
							morph.base.equals("前") | morph.base.equals("後") |
								morph.base.equals("まで") | morph.base.equals("から")
						} => "場所（時）（点）" //"Time-point"
						case _ => ""
					}
				case _ => ""
			}
		}
		return time
	}

	private def parseLocation(chunk: Chunk): String = {
		val location: String = chunk.category.contains("場所") match {
			case true => "場所" //"Location"
			case false => ""
		}
		return location
	}

	private def parseScene(chunk: Chunk): String = {
		val scene: String = chunk.category.contains("動作") match {
			case true =>
				chunk.morphs.exists { chunk =>
					chunk.surface.equals("に") || chunk.surface.equals("で")
				} match {
					case true => "場所（抽出）" //"Scene"
					case false => ""
				}
			case false => ""
		}
		return scene
	}

	private def parseInstrument(chunk: Chunk): String = {
		val instrument: String = chunk.category.contains("モノ") match {
			case true =>
				chunk.morphs.exists(_.surface.equals("で")) match {
					case true => "手段" //"Instrument"
					case false => ""
				}
			case _ => ""
		}
		return instrument
	}

	private def parseReason(chunk: Chunk): String = {
		val reason: String = chunk.morphs.exists { morph =>
			morph.surface.equals("ので") || morph.surface.equals("で")
		} match {
			case true => "原因" //"Reason"
			case false => ""
		}
		return reason
	}

	private def parseLimit(chunk: Chunk): String = {
		val limit: String = chunk.category.contains("数値") match {
			case true => chunk.morphs.exists(_.surface.equals("で")) match {
				case true => "限界" //"Limit"
				case false => ""
			}
			case false => ""
		}
		return limit
	}

	private def parsePurpose(chunk: Chunk): String = {
		val purpose: String = chunk.morphs.exists(_.surface.equals("ため")) match {
			case true =>
				chunk.modifiedchunks.foreach { modchunk =>
					modchunk.morphs.exists(_.surface.equals("の")) match {
						case true => modchunk.semrole = Seq("目的") //"Purpose" 
						case false => ""
					}
				}
				"目的" //"Purpose"
			case false => ""
		}
		return purpose
	}

	private def parseAs(chunk: Chunk): String = {
		val as: String = chunk.morphs match {
			case morphs if morphs.exists(_.surface.equals("として")) => "As"
			case _ => ""
		}
		return as
	}

	private def parseAround(chunk: Chunk): String = {
		val around: String = chunk match {
			case chunk if chunk.surface.equals("ことを") =>
				chunk.modifiedchunks.filter(_.morphs.last.surface.equals("の")).foreach(_.semrole = Seq("Around"))
				"Around"
			case chunk if chunk.morphs.exists(_.surface.equals("について")) => "Around"
			case _ => ""
		}
		return around
	}

	private def parsePremise(chunk: Chunk): String = {
		var premise: String = ""
		return premise
	}

	private def parseCitation(chunk: Chunk): String = {
		val citation: String = chunk.morphs match {
			case morphs if morphs.exists(_.surface.contains("引用")) => "Citation"
			case _ => ""
		}
		return citation
	}

	private def parseModificand(chunk: Chunk): String = {
		var modify = ""
		return modify
	}

	private def parseManner(chunk: Chunk): String = {
		var manner: String = ""
		return manner
	}
}