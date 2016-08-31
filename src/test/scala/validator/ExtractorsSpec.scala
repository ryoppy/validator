package validator

import org.scalatest._

class ExtractorsSpec extends FunSuite {
  val params = Map("a" -> "1")
  test("string") {
    assert(string("a").run(params) == ValidationSuccess("1"))
    assert(string("b").run(params) == ValidationFailure("b" -> Seq(ValidationError("required"))))
  }
  test("numeric") {
    assert(int("a").run(params) == ValidationSuccess(1))
    assert(float("a").run(params) == ValidationSuccess(1f))
    assert(long("a").run(params) == ValidationSuccess(1L))
    assert(double("a").run(params) == ValidationSuccess(1d))
    assert(short("a").run(params) == ValidationSuccess(1: Short))
    assert(byte("a").run(params) == ValidationSuccess(1: Byte))
    
    assert(int("b").run(params) == ValidationFailure("b" -> Seq(ValidationError("required"))))
    assert(int("a").run(Map("a" -> Long.MaxValue.toString)) == ValidationFailure("a" -> Seq(ValidationError("int"))))
  }
  test("optional") {
    assert(optional(string("a")).run(params) == ValidationSuccess(Some("1")))
    assert(optional(string("z")).run(params) == ValidationSuccess(None))

    assert(optional(int("a")).run(Map("a" -> Long.MaxValue.toString)) == ValidationFailure("a" -> Seq(ValidationError("int"))))
  }
  test("seq") {
    assert(seq(string("a")).run(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Seq("1", "2")))
    assert(seq(string("b")).run(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Nil))

    assert(seq(int("a")).run(Map("a[0]" -> Long.MaxValue.toString)) == ValidationFailure("a" -> Seq(ValidationError("int"))))
  }
  test("apply") {
    assert(string("a").apply("1") == ValidationSuccess("1"))
    assert(int("a").apply("1") == ValidationSuccess(1))
    assert(float("a").apply("1") == ValidationSuccess(1f))
    assert(long("a").apply("1") == ValidationSuccess(1L))
    assert(double("a").apply("1") == ValidationSuccess(1d))
  }
}
