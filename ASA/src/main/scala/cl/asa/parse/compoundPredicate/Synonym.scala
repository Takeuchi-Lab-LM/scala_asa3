package cl.asa.parse.compoundPredicate

import cl.asa.yaml.compoundPredicate._
import cl.asa.yaml.filter._
import cl.asa.result._
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import cl.asa.yaml._

/**
 * Synonym クラス
 * 名詞複合述語を見つけ、その類語と同じ語義、意味役割を付与する
 *
 * ほとんど Hiuchi のコピペ
 * 見つけた慣用句に対して与える情報が違うだけ
 */
class Synonym(compoundPredicates: compoundPredicate.Dict, filters: filter.Dict2) {

	def parse(result: Result) {
		this.matchCompoundPredicate(result)
	}

	/**
	 * 形態素の木を作成
	 */
	private def graphify(result: Result) {
		graphifyAsSequence(result)
		graphifyAsDependency(result)
	}

	/**
	 * 形態素の並び順によるグラフ化
	 */
	private def graphifyAsSequence(result: Result) {
		val morphs = getMorphs(result)
		var premorph = morphs.head
		morphs.tail.foreach { morph =>
			morph.tree = morph.tree :+ premorph
			premorph = morph
		}
	}

	/**
	 * 係り受け関係によるグラフ化
	 */
	private def graphifyAsDependency(result: Result) {
		result.chunks.reverse.foreach { chunk =>
			chunk.modifiedchunks.foreach { modchunk =>
				chunk.morphs.head.tree = chunk.morphs.head.tree match {
					case tree if !tree.exists(_.eq(modchunk.morphs.last)) => tree :+ modchunk.morphs.last
					case tree => tree
				}
			}
		}
	}

	/**
	 * 空白や接続詞などの関係ない形態素を除いたグラフ化
	 * @todo 他にも除外する条件あり
	 */
	private def graphifyAsSkipped(result: Result) {
		val morphs = getMorphs(result)
		morphs.foreach { morph =>
			morph.tree.foreach { depmorph =>
				if (depmorph.pos.contains("接頭詞")) {
					morph.tree = morph.tree.union(depmorph.tree)
				}
			}
		}
	}

	private def getMorphs(result: Result): Seq[Morph] = {
		val morphs: Seq[Morph] = result.chunks.map(_.morphs).flatten
		return morphs
	}

	/**
	 * @todo すべての形態素を使用し連語の候補を取得した後，再びすべての形態素と候補でマッチングを行っている＜ーこれは無駄
	 */
	private def matchCompoundPredicate(result: Result) {
		val morphs = getMorphs(result)
		val candicates = getCandicate(morphs)

		candicates.foreach { compoundPredicate =>
			matchs(morphs, compoundPredicate.patterns) match {
				case Some(compoundPredicate_morph) =>
					setCompoundPredicate(compoundPredicate, compoundPredicate_morph)
				case _ =>
			}
		}
	}

	private def setCompoundPredicate(compoundPredicate: CompoundPredicate, morphs: Seq[Morph]) {
		val chunks = morphs.map(_.chunk).distinct

		/**
		 * 複合名詞述語のメイン部分の語義を上書き
		 * メイン述語以外の部分の意味役割を上書き
		 */
		val score = filtering() //複合名詞と語義のどちらを選ぶかのスコアを判定（未実装）
		score > 0.8 match {
			case true =>
				chunks.foreach { chunk =>
					chunk == chunks.last match {
						case true =>
							chunk.semantic = compoundPredicate.semantic
						case false =>
							chunk.idiom = compoundPredicate.entry
							chunk.phrase = compoundPredicate.phrase.toSeq
							chunk.semrole = Seq("慣用句")
							chunk.idiom_morph = morphs
							chunk.idiom_score = score

					}
				}
			case false =>
		}
	}

	private def filtering(): Float = {
		val score = 1F
		return score
	}

	private def getCandicate(morphs: Seq[Morph]): Seq[CompoundPredicate] = {
		val cand = morphs.map { morph =>
			compoundPredicates.dict.collect {
				case idiom if isMatchPattern(morph, idiom.patterns.last) => idiom
			}
		}.flatten.distinct
		return cand
	}

	/**
	 * 連語の同定
	 */
	private def matchs(morphs: Seq[Morph], patterns: Array[Pattern]): Option[Seq[Morph]] = {
		val idiom = patterns match {
			case patterns if patterns.isEmpty => Some(Seq.empty[Morph])
			case _ =>
				morphs.collect {
					case morph if isMatchPattern(morph, patterns.last) =>
						matchs(morph.tree, patterns.init) match {
							case Some(idiom) => Some(idiom :+ morph)
							case _ => None
						}
				}.collectFirst { case Some(idiom) => idiom }
		}
		return idiom
	}

	private def isMatchPattern(morph: Morph, pattern: Pattern): Boolean = {
		val bool: Boolean = pattern.cases.exists { idcase =>
			var bol = true
			if (idcase.base.nonEmpty) { bol = bol & idcase.base.equals(morph.base) }
			if (idcase.read.nonEmpty) { bol = bol & idcase.read.equals(morph.read) }
			if (idcase.pos.nonEmpty) { bol = bol & idcase.pos.equals(morph.pos) }
			bol
		}
		return bool
	}
}
