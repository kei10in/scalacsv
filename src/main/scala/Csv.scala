package scalacsv

import scala.util.parsing.combinator._

object Csv extends RegexParsers {
  def apply(input: String): List[List[String]] = parseAll(table, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }

  override def skipWhitespace = false

  def table: Parser[List[List[String]]] = repsep(row, row_delimiter)

  def row: Parser[List[String]] = repsep(field, field_delimiter)

  def field: Parser[String] = quoted_field | raw_field

  def quoted_field: Parser[String] =
    "\"" ~> rep(charSeq | newline | """[^"]""".r) <~ "\"" ^^ {
      _.mkString
    }

  def charSeq: Parser[String] = "\"\"" ^^^ "\""

  def raw_field: Parser[String] = """[^,\n\r]*""".r

  def row_delimiter: Parser[String] = newline

  def field_delimiter: Parser[String] = ","

  def newline: Parser[String] = "\r\n" | "\n" | "\r"
}
