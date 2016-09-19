package validator

import org.scalatest._

class ValidationResultSpec extends FunSuite {
  test("translateErrors") {
    def translate(key: String, args: Seq[String]): Option[String] =
      if (key == "int") Some("int only") else None

    val result1: ValidationResult[Int] = int("name")("foo")
    assert(result1.errors.head._2.head.message == "int")

    val result2 = result1.translateErrors(translate)
    assert(result2.errors.head._2.head.message == "int only")
  }
}
