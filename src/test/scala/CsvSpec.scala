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

}
