# Validator

[English](README.md)

## まだアルファ版です。

[![Travis CI](https://travis-ci.org/ryoppy/validator.svg?branch=master)](https://travis-ci.org/ryoppy/validator)

ValidatorはScalaでバリデーションするライブラリです。

## インストール

下の依存をbuild.sbt等に入れてください。

```
libraryDependencies += "com.github.ryoppy" %% "validator-core" % "0.0.1"
```

## 例

```scala
import validator._

case class Foo(a: String, b: Int)

val v1: Validation[Foo] = Validation(
  string("a") is minLength(1),
  int("b") is between(1, 3)
).as[Foo]

val result = validate(Map("a" -> "A", "b" -> "1"), v1)
assert(result == ValidationSuccess(Foo("A", 1)))

val result2 = validate(Map("a" -> "A", "b" -> "0"), v1)
assert(result2 == ValidationFailure("b" -> Seq(ValidationError("between", Seq("1", "3")))))
```

合成

```scala
val v1: Validation[String] = string("a") is minLength(1)
val v2: Validation[Int] = int("a") is lessThan(1)
val v3: Validation[Foo] = (v1 :: v2).as[Foo]
val v4: Validation[(String, Int)] = (v1 :: v2).asTuple
```

for式

```scala
val v1: Validation[Foo] = for {
  a <- string("a") is minLength(1)
  b <- int("b") is lessThan(1)
} yield Foo(a, b)
```

[他の例](core/src/test/scala/validator/ExampleSpec.scala).

[テストケース](core/src/test/scala/validator).

## Playframeworkの例

```
// POST
validate(req.body.asFormUrlEncoded, v1)

// GET
validate(req.queryString, v1)
```

validateメソッドは、`Map[String, Seq[String]]`[等](core/src/main/scala/validator/ValidationValue.scala)受け取れます。

## なぜ作ったのか?

playframeworkのFormクラスは合成できなかったり22の制限があるとか色々あるので。

## License

MIT