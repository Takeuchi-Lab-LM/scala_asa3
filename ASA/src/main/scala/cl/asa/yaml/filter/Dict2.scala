package cl.asa.yaml.filter
import org.yaml.snakeyaml._
import scala.io._
import java.io._

class Dict2(dic: String, yaml: String, snakeyaml:Yaml) {
	//val ifile = Source.fromFile(dic)

	val is = getClass().getClassLoader().getResource(".").eq(null) match {
		case true => getClass().getClassLoader().getResourceAsStream(dic)
		case false => new FileInputStream(dic)
	}
	val ifile:scala.io.BufferedSource = scala.io.Source.fromInputStream(is)

	val index: Map[String, (Int, Int)] = ifile.getLines.map { str =>
		val n = str.split("\\s+")
		n(0) -> (n(1).toInt, n(2).toInt)
	}.toMap

	def getFilter(entry: String): Option[Filter] = {
		val filters = index.get(entry) match {
			case Some(fp) =>
				val str = this.loadString(fp._1, fp._2)
				val data = snakeyaml.loadAs(str, classOf[Filter])
				Some(data)
			case None => None
		}
		return filters
	}

	private def loadString(start: Int, end: Int): String = {
		val is = getClass().getClassLoader().getResource(".").eq(null) match {
			case true => getClass().getClassLoader().getResourceAsStream(yaml)
			case false => new FileInputStream(yaml)
		}
		val ar = new Array[Byte](end - start - 2)
		is.skip(start + 2)
		is.read(ar)
		val str = new String(ar, "UTF-8").replaceAll("\n  ", "\n")
		is.close()
		return str
	}
}