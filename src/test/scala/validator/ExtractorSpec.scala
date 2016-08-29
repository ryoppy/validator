package validator

import org.scalatest._

class ExtractorSpec extends FunSuite {
  val params = Map("a" -> "1")
  test("string") {
    assert(string("a").execute(params) == ValidationSuccess("1"))
    // err
//    assert(string("b").execute(params) == ValidationFailure("b", Seq(ValidationError("required"))))
  }
  test("numeric") {
    assert(int("a").execute(params) == ValidationSuccess(1))
    assert(float("a").execute(params) == ValidationSuccess(1f))
    assert(long("a").execute(params) == ValidationSuccess(1L))
    assert(double("a").execute(params) == ValidationSuccess(1d))
    assert(short("a").execute(params) == ValidationSuccess(1: Short))
    assert(byte("a").execute(params) == ValidationSuccess(1: Byte))
    // err
//    assert(int("b").execute(params) == ValidationFailure("b", Seq(ValidationError("required"))))
//    assert(int("a").execute(Map("a" -> Long.MaxValue.toString)) == ValidationFailure("a", Seq(ValidationError("int"))))
  }
  test("optional") {
    assert(optional(string("a")).execute(params) == ValidationSuccess(Some("1")))
    assert(optional(string("z")).execute(params) == ValidationSuccess(None))

//    assert(optional(int("a")).execute(Map("a" -> Long.MaxValue.toString)) == ValidationFailure("a", Seq(ValidationError("int"))))
  }
  test("seq") {
    assert(seq(string("a")).execute(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Seq("1", "2")))
    assert(seq(string("b")).execute(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Nil))

//    assert(seq(int("a")).execute(Map("a[0]" -> Long.MaxValue.toString)) == ValidationFailure("a", Seq(ValidationError("int"))))
  }
  test("run") {
    assert(string("a").run("1") == ValidationSuccess("1"))
    assert(int("a").run("1") == ValidationSuccess(1))
    assert(float("a").run("1") == ValidationSuccess(1f))
    assert(long("a").run("1") == ValidationSuccess(1L))
    assert(double("a").run("1") == ValidationSuccess(1d))
  }
}
