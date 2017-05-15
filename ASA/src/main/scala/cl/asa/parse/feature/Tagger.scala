package cl.asa.parse.feature
/**
 * 	態や時制などの情報を付与するためのクラス
 */
import cl.asa.result._
import cl.asa.yaml.category._
import cl.asa.yaml._

class Tagger(ccharts: cchart.Dict2, categorys: category.Dict) {

	def parse(result: Result): Result = {
		result.chunks.foreach { chunk =>
			chunk.voice = parseVoice(chunk)
			chunk.tense = parseTense(chunk)
			chunk.polarity = parsePolarity(chunk)
			chunk.sentelem = parseSentElem(chunk)
			chunk.mood = parseMood(chunk)
			chunk.category = parseCategory(chunk)
			chunk.morphs.foreach { morph =>
				morph.forms = parseCchart(morph)
			}
		}
		return result
	}
	/**
	 * 文節中に名詞カテゴリが付与できるものがあればカテゴリの種類を返す
	 * @param morphs 文節中の形態素の配列
	 * @return カテゴリ
	 */
	private def parseCategory(linkchunk: Chunk): Array[String] = {
		var category: Array[String] = categorys.getCates(linkchunk.main)
		linkchunk.morphs.foreach { morph =>
			category = morph.pos match {
				case "名詞,接尾,助数詞" | "名詞,数" =>
					morph.surface.diff(Seq("年" , "月" , "日" , "時" , "分" , "秒")).nonEmpty match{
						case true => category :+ "時間"
						case false =>category :+ "数値"
					}
					/*
					morph.surface match {
						case "年" | "月" | "日" | "時" | "分" | "秒" => category :+ "時間"
						case _ => category :+ "数値"
					}*/
				case "名詞,固有名詞,人名" | "名詞,接尾,人名" => category :+ "人"
				case "名詞,固有名詞,地域" | "名詞,接尾,地域" => category :+ "場所"
				case "名詞,固有名詞,組織" => category :+ "組織"
				case _ => category
			}
		}
		return category.distinct
	}

	/**
	 * 文節態を解析し取得
	 * 付与する態
	 *  - ACTIVE:   能動態
	 *  - CAUSATIVE:使役態
	 *  - PASSIVE:  受動態
	 *  - POTENTIAL:可能態
	 */
	private def parseVoice(chunk: Chunk): String = {
		val voice: String = chunk match {
			case chunk if chunk.morphs.exists { morph =>
				(morph.base.equals("れる") || morph.base.equals("られる")) && morph.pos.contains("動詞,接尾")
			} => "PASSIVE"
			case chunk if chunk.morphs.exists { morph =>
				morph.base.equals("できる") && morph.pos.contains("動詞,自立")
			} => "POTENTIAL"
			case chunk if chunk.morphs.exists { morph =>
				(morph.base.equals("せる") && morph.pos.contains("動詞,接尾")) ||
					(morph.base.equals("もらう") || morph.base.equals("いただく")) && morph.pos.contains("動詞,非自立")
			} => "CAUSATIVE"
			case chunk if !chunk.ctype.equals("elem") => "ACTIVE"
			case _ => ""
		}
		return voice
	}

	/**
	 * 時制情報の解析と付与
	 * 付与する時制の情報
	 *  - PAST:過去
	 */
	private def parseTense(chunk: Chunk): String = {
		val tense: String = chunk.morphs match {
			case morphs if morphs.exists { morph =>
				morph.pos.contains("助動詞") &&
					(morph.base.equals("た") /*|| (morph.base.equals("だ") && !morph.ctype.equals("体言接続"))*/ || morph.base.equals("き") || morph.base.equals("けり"))
			} => "PAST"
			case _ => "PRESENT"//saitoh 2016/09/06 "" -> "PRESENT"
		}
		return tense
	}

	/**
	 * 極性情報の解析と取得
	 * 付与する極性の情報
	 * AFFIRMATIVE:肯定
	 * NEGATIVE:   否定
	 */
	private def parsePolarity(chunk: Chunk): String = {
		val polarity: String = chunk match {
			case chunk if chunk.morphs.exists { morph =>
				morph.pos.contains("助動詞") &&
					(morph.base.equals("ない") || morph.base.equals("ぬ") || morph.base.contains("まい"))
			} => "NEGATIVE"
			case chunk if !chunk.ctype.equals("elem") => "AFFIRMATIVE"
			case _ => ""
		}
		return polarity
	}

	/**
	 *  形態素の活用型の情報を解析し取得"
	 *
	 */
	private def parseCchart(morph: Morph): Array[String] = {
		val forms: Array[String] = morph.cform match {
			case cform if cform.nonEmpty =>
				ccharts.getCchart(cform) match {
					case Some(cchart) => cchart.form
					case None => null
				}
			case _ => null
		}
		return forms
	}

	/**
	 * 文要素の情報を解析し取得
	 */
	private def parseSentElem(chunk: Chunk): String = {
		val sentelem: String = chunk.morphs.last match {
			case last if last.cform.contains("体言接続") | last.pos.contains("連体詞") | last.pos.contains("形容詞") |
				(last.pos.contains("助詞,連体化") && last.base.equals("の")) => "ADNOMINAL"
			case last if last.cform.contains("連用") || last.pos.contains("副詞") ||
				(last.base.equals("に") && last.pos.contains("助詞,格助詞")) => "ADVERBIAL"
			case _ if chunk.modifyingchunk.eq(null) => "PREDICATE"
			case _ => ""
		}
		return sentelem
	}

	/**
	 * 法情報を解析し取得
	 */
	private def parseMood(chunk: Chunk): String = {
		var mood: String = chunk.morphs.collect {
			case morph if morph.cform.equals("仮定") => "SUBJUNCTIVE"
			case morph if morph.cform.equals("命令") => "IMPERATIVE"
			case morph if morph.base.equals("な") & morph.pos.contains("助詞,終助詞") => "PROHIBITIVE"
			case morph if morph.base.equals("たい") & morph.pos.contains("助動詞") => "PROHIBITIVE"
			case morph if morph.base.equals("？") |
				(morph.base.equals("か") && morph.pos.contains("／")) => "INTERROGATIVE "
		} match {
			case mod if mod.nonEmpty => mod.distinct.mkString(",")
			case _ if !chunk.ctype.equals("elem") => "INDICATIVE"
			case _ => ""
		}
		return mood
	}
}