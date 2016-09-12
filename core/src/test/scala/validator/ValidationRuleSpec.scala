package validator

import org.scalatest._

class ValidationRuleSpec extends FunSuite {
  val params = Map("a" -> "1")
  test("apply") {
    assert(minLength(3).apply("abc"))
  }

  test("run") {
    assert(minLength(3).run("abc") == Right("abc"))
    assert(minLength(3).run("ab") == Left(ValidationError("minLength", Seq("3"))))
  }

  test("and") {
    assert(minLength(3).and(maxLength(5)).run("abc") == Right("abc"))
    assert(minLength(3).and(maxLength(5)).run("abcde") == Right("abcde"))
    assert(minLength(3).and(maxLength(5)).run("abcdef") == Left(ValidationError("maxLength", Seq("5"))))
    assert(minLength(3).and(maxLength(5)).run("ab") == Left(ValidationError("minLength", Seq("3"))))
    assert(minLength(3).and(maxLength(5).and(equiv("foo"))).run("foo") == Right("foo"))
    assert(minLength(3).and(maxLength(5).and(equiv("foo"))).run("fooz") == Left(ValidationError("equiv", Seq("foo"))))
  }

  test("withMessage") {
    assert(minLength(3).withMessage("this is error messages.").run("a") == Left(ValidationError("this is error messages.", Seq("3"))))
  }
}
