package cl.asa.parse.idiom
/**
 * 慣用句同定のためのクラス
 * 以下の手順により同定
 * - 形態素のグラフ化
 * - 慣用句の同定
 * - フィルタリング
 *
 * @todo 動作を優先させたためコードが汚いのでリファクタリングの必要アリ
 */
import cl.asa.yaml.idiom._
import cl.asa.yaml.filter._
import cl.asa.result._
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import cl.asa.yaml._

class Hiuchi(idioms: idiom.Dict, filters: filter.Dict2) {

	def parse(result: Result) {
		this.graphify(result)
		this.matchIdiom(result)
	}

	/**
	 * 慣用句同定のために入力文グラフを作成
	 *
	 */
	private def graphify(result: Result) {
		this.graphifyAsSequence(result)
		this.graphifyAsDependency(result)
		//graphifyAsSkipped(result)
	}
	/**
	 * 形態素の並び順によるグラフ化
	 */
	private def graphifyAsSequence(result: Result) {
		val morphs = getMorphs(result)
		morphs.reduceLeft { (prechunk, postchunk) =>
			postchunk.tree = postchunk.tree :+ prechunk
			postchunk
		}
	}
	/**
	 * 係り受け関係によるグラフ化
	 */
	private def graphifyAsDependency(result: Result) {
		result.chunks.foreach { chunk =>
			val modifiedmorphs: Seq[Morph] = chunk.modifiedchunks.map(_.morphs.last)
			chunk.morphs.head.tree = chunk.morphs.head.tree.union(modifiedmorphs).distinct
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
				if (depmorph.pos.contains("接頭詞") | depmorph.pos.contains("記号")
					| depmorph.pos.contains("接続助詞")) {
					morph.tree = morph.tree.union(depmorph.tree).distinct
				}

			}
		}
	}

	/**
	 * 慣用句表記辞書との比較し，慣用句情報の付与
	 */
	private def matchIdiom(result: Result) {
		val morphs: Seq[Morph] = getMorphs(result)
		val candicates: Seq[Idiom] = getCandicate(morphs)
		candicates.foreach { idiom =>
			matchMorphs(morphs, idiom.patterns).foreach { idiommorphs =>
				this.setIdiom(idiom, idiommorphs)
			}
		}
	}

	/**
	 * 慣用句表記辞書より候補となる慣用句の取得
	 * (慣用句の最後の形態素と一致する形態素があれば候補とする)
	 */
	private def getCandicate(morphs: Seq[Morph]): Seq[Idiom] = {
		val candicate: Seq[Idiom] = morphs.flatMap { morph =>
			idioms.dict.filter(idiom => isMatchPattern(morph, idiom.patterns.last))
		}.distinct
		return candicate
	}

	/**
	 * 慣用句の候補と入力文グラフを比較し，慣用句と一致する形態素を取得
	 */
	private def matchMorphs(morphs: Seq[Morph], patterns: Array[Pattern]): Seq[Seq[Morph]] = {
		val idiommorphs = patterns.foldRight(Seq.empty[Seq[Morph]]) { (pattern, precandicates) =>
			val candicates = precandicates.isEmpty match {
				case true => morphs.map(Seq(_)) //ture=>初期設定として形態素ごとにをSeqに入れる(候補がなくなったときもここに入ってしまう)
				case false =>
					precandicates.flatMap { precandicate => //false=>グラフの1つ先を追加
						precandicate.head.tree.map(morph => morph +: precandicate)
					}
			}
			candicates.filter(candicate => isMatchPattern(candicate.head, pattern)) //表記辞書との比較
		}.filter(_.size == patterns.size)
		return idiommorphs
	}

	/**
	 * 同定された慣用句の特徴をまとめるクラス
	 * この特徴は慣用句に関係する文節よりもってくる
	 */
	class mIdiom {
		var entry: String = ""
		var voice: Seq[String] = Seq.empty
		var polarity: Seq[String] = Seq.empty
		var mood: Seq[String] = Seq.empty
		var category: Seq[String] = Seq.empty
		var sentlem = ""
		var score = 0.0F
	}

	private def setIdiom(idiom: Idiom, morphs: Seq[Morph]) {
		val chunks = morphs.map(_.chunk).distinct
		val midiom = new mIdiom
		midiom.entry = idiom.entry
		chunks.foreach { chunk =>
			midiom.voice :+= chunk.voice
			midiom.mood :+= chunk.mood
			midiom.polarity :+= chunk.polarity
		}
		val modifer = chunks.flatMap(_.modifiedchunks).diff(chunks)
		midiom.category = modifer.collect {
			case chunk if chunk.category.nonEmpty => chunk.category
		}.flatten
		filtering(midiom)
		chunks.foreach { chunk =>
			chunk.idiom = idiom.entry
			chunk.phrase = idiom.phrase.toSeq
			chunk.idiom_morph = morphs
			chunk.idiom_score = midiom.score
		}
	}
	/**
	 * フィルタリング辞書より曖昧性のスコアを計算
	 */
	private def filtering(idiom: mIdiom) {
		val score = filters.getFilter(idiom.entry) match {
			case Some(filter) =>
				val nega = disambiguator(filter.negative, idiom) match {
					case true => 0.0F
					case false => 0.5F
				}
				val posi = disambiguator(filter.positive, idiom) match {
					case true => 1.0F
					case false => 0.5F
				}
				(nega + posi) / 2
			case None => 0.5F
		}
		idiom.score = score
	}

	/**
	 * フィルタリング辞書のposi/nega要素の一致判定
	 */
	private def disambiguator(feature: Feature, idiom: mIdiom): Boolean = {
		var bool: Boolean = false
		if (feature.polarity.nonEmpty) { bool |= feature.polarity.equals(idiom.polarity) }
		if (feature.sentelem.nonEmpty) {}
		if (feature.category.nonEmpty) { bool |= feature.category.exists(idiom.category.contains(_)) }
		if (feature.mood.nonEmpty) { bool |= feature.mood.exists(idiom.mood.contains(_)) }
		if (feature.voice.nonEmpty) { bool |= feature.voice.exists(idiom.voice.contains(_)) }
		return bool
	}

	/**
	 * resultよりすべての形態素を取得
	 */
	private def getMorphs(result: Result): Seq[Morph] = {
		val morphs: Seq[Morph] = result.chunks.map(_.morphs).flatten
		return morphs
	}

	/**
	 * 慣用句表記辞書内の1形態素分の一致判定
	 */
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