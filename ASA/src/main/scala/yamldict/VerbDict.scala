package yamldict
import scala.io.Source
import scala.math._
import cl.asa.yaml._
import cl.asa.init._
import fileIO._

/**
 *  述語項構造シソーラスよりフレーム辞書の作成
 */
class VerbDict(categorys: category.Dict) {

	def createDict(lines: Iterator[String]): Dict = {
		lines.next
		val dict = this.createVerbDict(lines)
		this.calculateWeight(dict)
		this.parseCaseConversion(dict)
		return dict
	}

	/**
	 * Dictクラス(動詞をまとめたクラス)の作成
	 */
	private def createVerbDict(lines: Iterator[String]): Dict = {
		val dict = new Dict
		for (line <- lines) {
			val vth = new CsvReader().parseLine(line).get
			val verb = vth(2) //述語
			val sem = vth.slice(30, 35).mkString("-") //概念フレーム
			vth(36).contains("×") match { //コメントに"☓"があれば除外
				case false =>
					dict.frames.find(frame => frame.verb.equals(verb)) match { //すでに述語のクラスが存在するか
						case Some(frame) =>
							frame.semantics.find(semantic => semantic.semantic.equals(sem)) match { //すでに概念フレームのクラスが存在するか
								case Some(semantic) =>
									semantic.instances = semantic.instances :+ this.createInstance(vth)
								case None =>
									frame.semantics = frame.semantics :+ this.createSemantic(vth)
							}
						case None =>
							dict.frames = dict.frames :+ this.createFrame(vth)
					}
				case true => println(vth(0) + vth(2))
			}
		}
		return dict
	}

	/**
	 * Frameクラス(動詞ごとに語義をまとめたクラス)の作成
	 */
	private def createFrame(vth: List[String]): Frame = {
		val frame = new Frame
		frame.verb = vth(2)
		frame.semantics = frame.semantics :+ this.createSemantic(vth)
		return frame
	}

	/**
	 * Semanticクラス(語義ごとに事例をまとめたクラス)の作成
	 */
	private def createSemantic(vths: List[String]): Semantic = {
		val semantic = new Semantic
		semantic.semantic = vths.slice(30, 35).mkString("-")
		semantic.instances = semantic.instances :+ this.createInstance(vths)
		return semantic
	}

	/**
	 * Instanceクラス(事例ごとに格をまとめたクラス)の作成
	 */
	private def createInstance(vths: List[String]): Instance = {
		val instance = new Instance()
		for (n <- List(4, 9, 14, 19, 24)) {
			val role = vths(n) //深層格
			val parts = vths(n + 1).split("・").toList.filterNot(_.contains("?")) //表層格
			val noun = vths(n + 2).replace(parts(0), "") //事例
			val cates = categorys.getCates(noun) match {
				case cat if cat.isEmpty => Array("NoData")
				case cat => cat
			} //カテゴリ
			parts.foreach { part =>
				cates.foreach { cate =>
					val cas = this.createCase(role, part, noun, cate)
					if (role.nonEmpty) instance.cases = instance.cases :+ cas
				}
			}
		}
		return instance
	}

	/**
	 * Caseクラス(格をまとめたクラス)の作成
	 */
	private def createCase(role: String, part: String, noun: String, category: String): Case = {
		val cas = new Case
		cas.semrole = role
		cas.part = part
		cas.noun = noun
		cas.category = category
		return cas
	}

	/**
	 * 重みの計算
	 */
	private def calculateWeight(dict: Dict) {
		dict.frames.foreach { frame =>
			frame.semantics.foreach { semantic =>
				semantic.instances.foreach { instance =>
					instance.cases.foreach { cas =>
						val key = cas.category + cas.part
						val local_weight = this.calculateLocalWeight(semantic.instances, key)
						val grobal_weight = this.calculateGrobalWeight(frame.semantics, semantic.instances, key)
						cas.weight = local_weight * grobal_weight
					}
				}
			}
		}
	}

	/**
	 * 同語義事例間重みの計算
	 */
	private def calculateLocalWeight(instances: List[Instance], key: String): Float = {
		val tf = 1
		val df = instances.count(instance => instance.cases.exists(cas => key.equals(cas.category + cas.part)))
		val idf = (log(instances.size / df) / log(2)) + 1
		val weight = 1 / (tf * idf)
		return weight.toFloat
	}

	/**
	 * 別語義事例間重みの計算
	 */
	private def calculateGrobalWeight(semantics: List[Semantic], instances: List[Instance], key: String): Float = {
		val tf = instances.count(instance => instance.cases.exists(cas => key.equals(cas.category + cas.part)))
		val df = semantics.count(semantic => semantic.instances.exists(instance => instance.cases.exists(cas => key.equals(cas.category + cas.part))))
		val idf = (log(semantics.size / df) / log(2)) + 1
		val weight = tf * idf
		return weight.toFloat
	}

	/**
	 * 作成した辞書に助詞の変換パターンを付与
	 */
	private def parseCaseConversion(dict: Dict) {
		val senpats = Source.fromFile("/home1/ex/ikeda/data/pattern.csv")
		val patterns = this.createPatternDic(senpats.getLines)
		dict.frames.foreach { frame =>
			frame.semantics.foreach { semantic =>
				semantic.instances.foreach { instance =>
					val parts: String = instance.cases.map(cas => cas.part).mkString("")
					patterns.get(parts) match {
						case Some(pattern) =>
							instance.cases.foreach { cas =>
								if (cas.part.nonEmpty) {
									cas.causative_part = pattern.apply("causative").apply(cas.part)
									cas.passive_part = pattern.apply("passive").apply(cas.part)
								}
							}
						case None =>
					}
				}
			}
		}
	}

	/**
	 * 助詞の変換パターンをまとめた辞書を読み込み
	 */
	private def createPatternDic(lines: Iterator[String]): Map[String, Map[String, Map[String, String]]] = {
		var patterns: Map[String, Map[String, Map[String, String]]] = Map.empty
		for (line <- lines) {
			val csv = new CsvReader().parseLine(line).get
			var pattern: Map[String, Map[String, String]] = Map.empty
			if (csv(9).nonEmpty) {
				//使役態の助詞変換パターンの読み込み
				val elems: Array[String] = csv(9).split(';').apply(0).split(':')
				var causatives: Map[String, String] = Map.empty
				elems.foreach { elem =>
					val part = elem.split("=>")
					causatives = causatives.updated(part(0), part(1))
				}
				pattern = pattern.updated("causative", causatives)

				//受動態の助詞変換パターンの読み込み
				val elems2: Array[String] = csv(10).split(';').apply(0).split(':')
				var passives: Map[String, String] = Map.empty
				elems2.foreach { elem =>
					val part = elem.split("=>")
					passives = passives.updated(part(0), part(1))
				}
				pattern = pattern.updated("passive", passives)

				patterns = patterns.updated(csv(0), pattern)
			}
		}
		return patterns
	}

	class Dict {
		var frames = List.empty[Frame]
	}

	class Frame {
		var verb: String = ""
		var semantics = List.empty[Semantic]
	}

	class Semantic {
		var semantic: String = ""
		var instances = List.empty[Instance]
	}
	class Instance {
		var cases = List.empty[Case]
	}
	class Case {
		var part: String = ""
		var causative_part = ""
		var passive_part = ""
		var noun: String = ""
		var category: String = ""
		var semrole: String = ""
		var weight: Float = 0F
	}
}