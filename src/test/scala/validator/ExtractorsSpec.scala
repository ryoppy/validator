package validator

import org.scalatest._

class ExtractorsSpec extends FunSuite {
  val params = Map("a" -> "1")
  test("string") {
    assert(string("a").run(params) == ValidationSuccess("1"))
    assert(string("b").run(params) == ValidationFailure.of("b" -> Seq(ValidationError("required"))))
  }
  test("numeric") {
    assert(int("a").run(params) == ValidationSuccess(1))
    assert(float("a").run(params) == ValidationSuccess(1f))
    assert(long("a").run(params) == ValidationSuccess(1L))
    assert(double("a").run(params) == ValidationSuccess(1d))
    assert(short("a").run(params) == ValidationSuccess(1: Short))
    assert(byte("a").run(params) == ValidationSuccess(1: Byte))
    
    assert(int("b").run(params) == ValidationFailure.of("b" -> Seq(ValidationError("required"))))
    assert(int("a").run(Map("a" -> Long.MaxValue.toString)) == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
  }
  test("boolean") {
    assert(boolean("a").run(Map("a" -> "true")) == ValidationSuccess(true))
    assert(boolean("a").run(Map("a" -> "ok")) == ValidationSuccess(true))
    assert(boolean("a").run(Map("a" -> "1")) == ValidationSuccess(true))

    assert(boolean("a").run(Map("a" -> "foo")) == ValidationFailure.of("a" -> Seq(ValidationError("boolean"))))
  }
  test("jodaDate") {
    assert(jodaDateTime("a").run(Map("a" -> "2016-01-01")) == ValidationSuccess(org.joda.time.DateTime.parse("2016-01-01")))
  }
  test("jodaLocalDate") {
    assert(jodaLocalDate("a").run(Map("a" -> "2016-01-01")) == ValidationSuccess(org.joda.time.LocalDate.parse("2016-01-01")))
  }
  test("localDate") {
    assert(localDate("a").run(Map("a" -> "2016-01-01")) == ValidationSuccess(java.time.LocalDate.parse("2016-01-01")))
  }
  test("localDateTime") {
    assert(localDateTime("a").run(Map("a" -> "2016-01-01 01:02:03")) == ValidationSuccess(java.time.LocalDateTime.parse("2016-01-01T01:02:03")))
  }
  test("localTime") {
    assert(localTime("a").run(Map("a" -> "01:02:03")) == ValidationSuccess(java.time.LocalTime.parse("01:02:03")))
  }
  test("uuid") {
    assert(uuid("a").run(Map("a" -> "49b12b2e-3be9-4f20-be03-9fd1ab7919d4")) == ValidationSuccess(java.util.UUID.fromString("49b12b2e-3be9-4f20-be03-9fd1ab7919d4")))
  }
  test("optional") {
    assert(optional(string("a")).run(params) == ValidationSuccess(Some("1")))
    assert(optional(string("z")).run(params) == ValidationSuccess(None))

    assert(optional(int("a")).run(Map("a" -> Long.MaxValue.toString)) == ValidationFailure.of("a" -> Seq(ValidationError("int"))))
  }
  test("seq") {
    assert(seq(string("a")).run(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Seq("1", "2")))
    assert(seq(string("b")).run(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Nil))

    assert(seq(int("a")).run(Map("a[0]" -> Long.MaxValue.toString)) == ValidationFailure.of("a[0]" -> Seq(ValidationError("int"))))
    assert(seq(int("a")).run(Map("a[0]" -> "1", "a[1]" -> Long.MaxValue.toString)) == ValidationFailure.of("a[1]" -> Seq(ValidationError("int"))))
    assert(seq(int("a")).run(Map("a[0]" -> Long.MaxValue.toString, "a[1]" -> Long.MaxValue.toString)) == ValidationFailure.of("a[0]" -> Seq(ValidationError("int")), "a[1]" -> Seq(ValidationError("int"))))
  }
  test("set") {
    assert(set(string("a")).run(Map("a[0]" -> "1", "a[1]" -> "1")) == ValidationSuccess(Set("1")))
    assert(set(string("b")).run(Map("a[0]" -> "1", "a[1]" -> "2")) == ValidationSuccess(Set()))

    assert(set(int("a")).run(Map("a[0]" -> Long.MaxValue.toString)) == ValidationFailure.of("a[0]" -> Seq(ValidationError("int"))))
  }
  test("apply") {
    assert(string("a").apply("1") == ValidationSuccess("1"))
    assert(int("a").apply("1") == ValidationSuccess(1))
    assert(float("a").apply("1") == ValidationSuccess(1f))
    assert(long("a").apply("1") == ValidationSuccess(1L))
    assert(double("a").apply("1") == ValidationSuccess(1d))
  }
}
