## 目的

PlayのFormが使いにくいのでValidation用のライブラリを作る。
SkinnyValidatorがあるが、型レベルで色々作りたい。

## API

```
type ValidationRules = ValidationRule[T] :: HNil
ValidationRule[String](_ + "is required")(_ != "")

xs = Validation[User](
  value("name") is required and minLength(3) ::
  value("age") is required
)

validate()

Validator(Map("name" -> "foo"))(
  value("name") is required and minLength(3)
)
```

## 受け取るデータに依存しない

- データを受け取る。
  - Map[String, Any]
  - case class
  - (String, Any)
  - ネスト (foo.name, foo.bar.name)
  - 配列 (foo[])
- 各対象の値にバリデーションルールを追加する。
  - and/or
  - compose
- バリデーションする。

### データを受け取る

- key=1&key[]=2&key.foo=3
- Map(key -> 1, key[0] -> 2, key.foo -> 3]

type ItemName = String
type RuleName = String

trait ValidationRule[A] { self: =>
  def name: RuleName // min
  def apply(x: A): Boolean

  def execute(x: A): ValidationResult[A] = ValidationResult(name, x, self(x))
  def and(next: ValidationRule[A]): ValidationRule[A] =
    new ValidationRule[A]
      def name = self.name
      def execute(x: A): ValidationResult[A] =
        val x1 = self.execute(x)
        if (x1.isSuccess) next(x)
        else x1
  def or
    new ValidationRule[A]
      def name = self.name
      def execute(x: A): ValidationResult[A] =
        val x1 = self.execute(x)
        if (x1.isSuccess) next(x)
        else
          val x2 = self.execute(x)
          if (x2.isSuccess) x2
          else x1

case class ValidationResult[A](
  name: RuleName,
  value: A,
  isSuccess: Boolean
) {
  def isError = !isSuccess
}

def validate[P: Params, A <: HList](params: P)(x: A): ValidationResult[A]

// HListなしでも渡せるように
def validate[A-22](a: , b: ,c: ...)


trait ValidationItem[A]
  val name: ItemName
  val rules: Seq[ValidationRule[A]]
  def verifying(b: ValidationRule[A]): ValidationItem[A] = ???

def item[A](name: String): ValidationItem[A]
def item(name: String): ValidationItem[String]
def as[A: Generic]() // 型は指定しなくてもcase classで自動であわせたい

item (required or min) and (min or max)


validate(params) {
  item("name").verifying(required and minLength(3)) ::
  item("name") is required and minLength(3) ::
  item[Int]("name") is required and minLength(3)
} // String :: Int

from(map) {
  (param("name") is required and minLength(3) ::
  param("name") is required and minLength(3)).as[User]
}
// ValidationParam[String :: String :: HNil] を as[User] で
// ValidationParam[String :: Int :: HNil] に自動で型合わせできるといい?



def param()

def param[A: ValidationParamConverter](key: String): ValidationParam[String] = new ValidationParam[String] {
  val key: String = key
}

trait ValidationParam[A: ValidationParamConverter]
  val key: String
  val rules: Seq[ValidationRule[A]] = Nil

  val is = this
  def and(x: ValidationRule[A]): ValidationParam[A] = new ValidationParam[]
  def or(x: ValidationRule[A]): ValidationParam[A]
  def validate(x: A): Either[Seq[Error], A] =
    rules.map { rule =>
      if (rule.run(value)) Right(value) else Left()
    }.fold





def from[T: ValidationParams](x: T) = ???

implicit val vpMap = new ValidationParams[Map[String, Any]] {
  def apply(x: Map[String, Any]): Seq[ValidationParam[Any]] =
}




trait ValidationRule[A]

trait ValidationResult[A]
