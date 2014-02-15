package scalacsv

import org.scalatest.FunSuite

class HelloWorldSpec extends FunSuite {

  def parse(s: String) = Csv.parse(s)

  test("parse scalar") {
    val actual = parse("1")
    assert(actual === List(List("1")))
  }

}
