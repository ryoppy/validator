package validator

sealed trait ValidationResult[A] {
  self =>

  def map[B](f: A => B): ValidationResult[B]

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B]

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B]

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B
}

object ValidationResult {
  type Error = (ValidationName, Seq[ValidationError])
}

final case class ValidationSuccess[A](value: A) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationSuccess(f(value))

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = f(value)

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B] = ValidationSuccess(value)

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B = g(value)
}

final case class ValidationFailure[A](errors: ValidationResult.Error*) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationFailure[B](errors: _*)

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = ValidationFailure[B](errors: _*)

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B] = that

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B = f(errors)
}
