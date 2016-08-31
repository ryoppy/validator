# Validator

Validator is a Validation library for Scala.

## Example

```scala
import validator._

case class Foo(a: String, b: Int)

val v1: Validation[Foo] = Validation(
  string("a") is minLength(1),
  int("b") is min(1) and max(3)
).as[Foo]

val result = validate(Map("a" -> "A", "b" -> "1"), v1)
assert(result == ValidationSuccess(Foo("A", 1)))

val result2 = validate(Map("a" -> "A", "b" -> "0"), v1)
assert(result2 == ValidationFailure("b" -> Seq(ValidationError("min", Seq("1")))))
```

[more examples](./).

## QuickStart

## Why?

## License

MIT