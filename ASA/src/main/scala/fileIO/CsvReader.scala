package fileIO
import scala.util.parsing.combinator._

class CsvReader extends RegexParsers {
	override val skipWhitespace = false

	//No Double quote field 
	def normalField = "[^,\r\n]*".r

	//Double quote field
	def quoteField = dblquote ~> (("[^\"]".r | escDblquote).* ^^ (x => x.mkString)) <~ dblquote
	def dblquote = "\""

	//escape double quote
	def escDblquote = "\"\"" ^^ (x => "\"")

	//process for a line
	def fields = repsep(quoteField | normalField, ",")

	// process for lines 
	def lines = repsep(fields | fields, eol)
	def eol = "\r\n" | "\n" | "\r"
	def parseLine(input: String) = parseAll(fields, input)
	def parse(input: String) = parseAll(lines, input)

}