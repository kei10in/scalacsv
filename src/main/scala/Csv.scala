package scalacsv

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader.EofCh

object Csv extends RegexParsers {
  def apply(input: String): List[List[String]] = parseAll(table, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }

  def table: Parser[List[List[String]]] = rep(row)

  def row: Parser[List[String]] = (field ~ rep(continuous_field)) ~ opt(newline) ^^ {
    case x ~ y ~ _ => x :: y
  }

  def newline: Parser[String] = """\n""".r

  def continuous_field: Parser[String] = "," ~> field

  def field: Parser[String] = quoted_field | raw_field

  def quoted_field: Parser[String] =
    "\"" ~> rep("""[^"]""".r | charSeq) <~ "\"" ^^ {
      _.mkString
    }

  def charSeq: Parser[String] = '"' ~ '"' ^^^ "\""

  def raw_field: Parser[String] = """[^,\n]+""".r

}
