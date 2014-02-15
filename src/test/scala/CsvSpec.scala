package scalacsv

import org.scalatest.FunSuite

class CsvSpec extends FunSuite {

  test("parse scalar") {
    val actual = Csv("1")
    assert(actual === List(List("1")))
  }

  test("parse simple 10 fields") {
    val actual = Csv("0,1,2,3,4,5,6,7,8,9")
    assert(actual === List(List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")))
  }

}
