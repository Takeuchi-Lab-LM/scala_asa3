package cl.asa.parse
/**
 * ASAの解析のためのクラス
 */
import cl.asa.parse.cabocha._
import cl.asa.result._
import cl.asa.yaml._
import cl.asa.parse.feature._
import cl.asa.parse.idiom._
import cl.asa.parse.compoundPredicate._

class Parse(dicts: LoadYaml) {
	private val cabocha = new Cabocha("cabocha -f1 -n1", "utf-8")
	private val basic = new Basic()
	private val sematter = new semantic.Sematter(dicts.getFrames, dicts.getCategorys, dicts.getNouns)
	private val tagger = new Tagger(dicts.getCcharts, dicts.categorys)
	private val idiom = new Hiuchi(dicts.getIdioms, dicts.getFilters)
	private val compoundPredicate = new Synonym(dicts.getCompoundPredicates, dicts.getFilters)
	basic.setFrames(dicts.getFrames)
	def parse(line: String): Result = {
		var result = this.parseChunk(line)
		result = this.parseFeature(result)
		result = this.parseIdiom(result)
		result = this.parseSemantic(result)
		result = parseCompoundPredicate(result)
		return result
	}

	
	/**
	 * cabochaを利用し文を文節と形態素を解析
	 * また解析結果より相互関係や動詞などの情報を整理
	 */
	private def parseChunk(line: String): Result = {
		var result = cabocha.parse(line)
		result = basic.parse(result)
		return result
	}

	/**
	 * 態や名詞カテゴリなどを付与
	 */
	private def parseFeature(result: Result): Result = {
		tagger.parse(result)
		return result
	}

	/**
	 * 慣用句の同定をおこない，フィルタリングをする
	 */
	private def parseIdiom(result: Result): Result = {
		idiom.parse(result)
		return result
	}

	/**
	 * 語義や意味役割の付与
	 */
	private def parseSemantic(result: Result): Result = {
		sematter.parse(result)
		return result
	}
	
	/**
	 * 複合述語の同定を行い、一部の語義と意味役割を上書きする
	 */
	def parseCompoundPredicate(result: Result): Result = {
		compoundPredicate.parse(result)
		return result
	}

	def end() {
		cabocha.end
	}
}

