package cl.asa.yaml
/**
 * yamlファイルの各辞書を読み込むクラス
 */
import org.yaml.snakeyaml._
import java.io._
import cl.asa.yaml.frame._
import cl.asa.yaml.category._
import cl.asa.yaml.cchart._
import cl.asa.yaml.verb._
import cl.asa.yaml.compoundPredicate._
import cl.asa.init._

class LoadYaml(files: YamlFile) {
	val snakeyaml = new Yaml()
	val frames: frame.Dict2 = this.loadFrames(files.dicframe, files.getFrame)
	val categorys: category.Dict = this.loadCategorys(files.getCategory)
	val ccharts: cchart.Dict2 = this.loadCcharts(files.diccchart, files.getCchart)
	val idioms: idiom.Dict = this.loadIdioms(files.getIdiom)
	val filters: filter.Dict2 = this.loadFilters(files.getFilter)
	val compoundPredicates: compoundPredicate.Dict = this.loadCompoundPredicates(files.getCompoundPredicate)
	val nouns:noun.Dict = this.loadNouns(files.getNoun)
	
	def getIdioms(): idiom.Dict = {
		return idioms
	}
	def getCategorys(): category.Dict = {
		return categorys
	}

	def getCcharts(): cchart.Dict2 = {
		return ccharts
	}

	def getFrames(): frame.Dict2 = {
		return frames
	}
	def getFilters(): filter.Dict2 = {
		return filters
	}
	def getCompoundPredicates():compoundPredicate.Dict = {
		return compoundPredicates
	}
	def getNouns():noun.Dict ={
		return nouns
	}
	

	private def loadIdioms(yaml: String): idiom.Dict = {
		val is = getClass().getClassLoader().getResource(".").eq(null) match {
			case true => getClass().getClassLoader().getResourceAsStream(yaml)
			case false => new FileInputStream(yaml)
		}
		val idioms = snakeyaml.loadAs(is, classOf[idiom.Dict])
		return idioms
	}

	private def loadCcharts(dic: String, yaml: String): cchart.Dict2 = {
		val ccharts = new cchart.Dict2(dic, yaml, snakeyaml)
		return ccharts
	}

	private def loadCategorys(yaml: String): category.Dict = {
		val is = getClass().getClassLoader().getResource(".").eq(null) match {
			case true => getClass().getClassLoader().getResourceAsStream(yaml)
			case false => new FileInputStream(yaml)
		}
		val categorys = snakeyaml.loadAs(is, classOf[category.Dict])
		return categorys
	}

	private def loadFrames(dic: String, yaml: String): frame.Dict2 = {
		val frames = new frame.Dict2(dic, yaml, snakeyaml)
		return frames
	}

	private def loadFilters(file: String): filter.Dict2 = {
		val filters = new filter.Dict2(files.dicfilter, file, snakeyaml)
		return filters
	}

	private def loadCompoundPredicates(yaml: String): compoundPredicate.Dict = {
		val is = getClass().getClassLoader().getResource(".").eq(null) match {
			case true => getClass().getClassLoader().getResourceAsStream(yaml)
			case false => new FileInputStream(yaml)
		}
		val compoundPredicates = snakeyaml.loadAs(is, classOf[compoundPredicate.Dict])
		return compoundPredicates
	}
	
	private def loadNouns(yaml:String):noun.Dict ={
		val is = getClass().getClassLoader().getResource(".").eq(null) match {
			case true => getClass().getClassLoader().getResourceAsStream(yaml)
			case false => new FileInputStream(yaml)
		}
		val nouns = snakeyaml.loadAs(is, classOf[noun.Dict])
		return nouns
	}
}