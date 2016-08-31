package validator

sealed trait ValidationResult[A] {
  self =>

  def map[B](f: A => B): ValidationResult[B]

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B]
}

final case class ValidationSuccess[A](value: A) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationSuccess(f(value))

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = f(value)
}

final case class ValidationFailure[A](errors: (ValidationName, Seq[ValidationError])*) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationFailure[B](errors: _*)

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = ValidationFailure[B](errors: _*)
}
