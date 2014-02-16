package scalacsv

import org.scalatest.FunSuite

class CsvSpec extends FunSuite {

  test("parse scalar") {
    val input = "1"
    assert(Csv(input) === List(List("1")))
  }

  test("parse simple 10 fields") {
    val input = "0,1,2,3,4,5,6,7,8,9"
    assert(Csv(input) === List(List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")))
  }

  test("parse simple rows") {
    val input
      = """0,1
          |3,4
          |5,6
          |""".stripMargin
    assert(Csv(input) === List(List("0", "1"), List("3", "4"), List("5", "6")))
  }

  test("parse quoted field") {
    val input = "\"1\""
    assert(Csv(input) === List(List("1")))
  }

  test("parse escaped quote") {
    val input = "\"\"\"\""
    assert(Csv(input) === List(List("\"")))
  }

  test("parse field including comma between quotes") {
    val input = "\"a,b\",c"
    assert(Csv(input) === List(List("a,b", "c")))
  }

  test("parse field including newline between quotes") {
    val input = "\"a\nb\",c"
    assert(Csv(input) === List(List("a\nb", "c")))
  }

  test("parse empty string as empty") {
    val input = ""
    assert(Csv(input) === Nil)
  }

 }
