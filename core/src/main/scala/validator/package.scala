package object validator extends ValidationRules with Extractors {
  type RuleName = String
  type ValidationName = String

  def validate[A, B](value: A, validation: Validation[B])(implicit vv: ValidationValue[A]): ValidationResult[B] = {
    validation.run(vv(value))
  }
}
