package scalacsv

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader.EofCh

object Csv extends RegexParsers {
  def apply(input: String): List[List[String]] = parseAll(table, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }

  override def skipWhitespace = false

  def table: Parser[List[List[String]]] = repsep(row, "\n")

  def row: Parser[List[String]] = repsep(field, ",")

  def field: Parser[String] = quoted_field | raw_field

  def quoted_field: Parser[String] =
    "\"" ~> rep(charSeq | newlineChar | """[^"]""".r) <~ "\"" ^^ {
      _.mkString
    }

  def newlineChar: Parser[String] = elem('\n') ^^^ "\n"

  def charSeq: Parser[String] = '"' ~ '"' ^^^ "\""

  def raw_field: Parser[String] = """[^,\n]*""".r

  def field_delimiter = ","
}
