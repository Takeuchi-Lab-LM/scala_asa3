package correctdata

class Extraction {
	val term: List[String] = List("見出し語", "格1(深層格)", "格1(事例)", "格2(深層格)", "格2(事例)", "格3(深層格)", "格3(事例)", "格4(深層格)", "格4(事例)", "格5(深層格)", "格5(事例)", "格6(深層格)", "格6(事例)", "動詞位置", "例文", "語義")

	/**
	 * 述語項構造シソーラスから比較実験に使用するCSVデータを作成
	 */
	def extractVth(vths: List[List[String]]): List[List[String]] = {
		var csvs: List[List[String]] = List.empty
		csvs :+= term
		vths.foreach { vth =>
			vth(36).contains("×") match {
				case false => csvs :+= createVthCsv(vth)
				case true => println(vth)
			}
		}
		return csvs
	}
	private def createVthCsv(vth: List[String]): List[String] = {
		var csv: List[String] = List.empty
		csv :+= vth(2) //見出し語
		List(4, 9, 14, 19, 24).foreach { n =>
			vth(n + 2).nonEmpty match {
				case true => csv ++= List(vth(n), vth(n + 2)) //(意味役割，文節)
				case false =>
			}
		}
		csv = csv.padTo(13, "")
		csv :+= "0" //位置
		csv :+= vth(29) //例文	
		csv :+= vth.slice(30, 35).mkString("-") //語義
		return csv
	}

	/**
	 * BCCWJに付与したデータから比較実験に使用するCSVデータを作成
	 */
	def extractBCCWJ(instances: List[List[List[String]]]): List[List[String]] = {
		var csvs: List[List[String]] = List.empty
		csvs :+= term
		instances.foreach { instance =>
			val correct = this.extractCorrect(instance)
			correct.verb.nonEmpty match {
				case true => csvs :+= this.createBCCWJCsv(correct)
				case false => println(correct.text)
			}
		}
		return csvs
	}

	private def createBCCWJCsv(correct: CorrectData): List[String] = {
		var csv: List[String] = List.empty
		csv :+= correct.verb
		correct.role.foreach { role =>
			csv ++= List(role._1, role._2)
		}
		csv = csv.padTo(13, "")
		csv :+= correct.position.toString
		csv :+= correct.text
		csv :+= correct.semnatic
		return csv
	}

	private def extractCorrect(instance: List[List[String]]): CorrectData = {
		val correct = new CorrectData
		correct.text = instance.map(_(1)).mkString
		instance.find(_(5).contains("-")) match { //概念フレームが付与された述語の検索
			case Some(morph) =>
				correct.verb = morph(2)
				correct.semnatic = morph(5)
			case None =>
		}
		correct.position = instance.filter(_(2).equals(correct.verb)).indexWhere(_(5).equals(correct.semnatic))
		correct.role = this.relateToRole(instance)

		return correct
	}
	/**
	 * 意味役割が付与された項の取得
	 */
	private def relateToRole(instance: List[List[String]]): Map[String, String] = {
		val roles: Map[String, String] = instance.filter { morph =>
			morph(5).nonEmpty & !morph(5).contains("-")
		}.foldLeft(Map.empty[String, String]) { (role, morph) =>
			role.updated(morph(5), role.getOrElse(morph(5), "") + morph(1))
		}
		return roles
	}
}