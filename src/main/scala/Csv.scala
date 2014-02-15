package scalacsv

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharArrayReader.EofCh

object Csv extends RegexParsers {
  def apply(input: String): List[List[String]] = parseAll(table, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }

  def table: Parser[List[List[String]]] = field ~ rep(continuous_field) ^^ {
    case x ~ y => List(x :: y)
  }

  def continuous_field: Parser[String] = "," ~> field

  def field: Parser[String] = """[^,]+""".r

}
