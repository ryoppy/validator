package validator

sealed trait ValidationResult[A] {
  self =>

  def map[B](f: A => B): ValidationResult[B]

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B]

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B]

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B

  def value: Option[A]

  def errors: Seq[ValidationResult.Error]

  def failureMap(f: Seq[ValidationResult.Error] => Seq[ValidationResult.Error]): ValidationResult[A]

  /**
   * translate validation error messages.
   *
   * {{{
   *   // example use play i18n
   *   def f(ruleName: String, args: Seq[String]): Option[String] =
   *     play.api.libs.i18n.Messages(ruleName, args:_*)
   *   
   *   result.translateErrors(f)
   * }}}
   */
  def translateErrors(f: (RuleName, Seq[String]) => Option[String]): ValidationResult[A]
}

object ValidationResult {
  type Error = (ValidationName, Seq[ValidationError])
}

final case class ValidationSuccess[A](x: A) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationSuccess(f(x))

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = f(x)

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B] = ValidationSuccess(x)

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B = g(x)

  val value: Option[A] = Some(x)

  val errors: Seq[ValidationResult.Error] = Nil
  
  def failureMap(f: Seq[ValidationResult.Error] => Seq[ValidationResult.Error]): ValidationResult[A] = this

  def translateErrors(f: (RuleName, Seq[String]) => Option[String]): ValidationResult[A] = this
}

final case class ValidationFailure[A](errors: Seq[ValidationResult.Error]) extends ValidationResult[A] {
  def map[B](f: A => B): ValidationResult[B] = ValidationFailure[B](errors)

  def flatMap[B](f: A => ValidationResult[B]): ValidationResult[B] = ValidationFailure[B](errors)

  def orElse[B >: A](that: ValidationResult[B]): ValidationResult[B] = that

  def fold[B](f: Seq[ValidationResult.Error] => B, g: A => B): B = f(errors)

  val value: Option[A] = None

  def failureMap(f: Seq[ValidationResult.Error] => Seq[ValidationResult.Error]): ValidationResult[A] =
    ValidationFailure[A](f(errors))

  def translateErrors(f: (RuleName, Seq[String]) => Option[String]): ValidationResult[A] =
    ValidationFailure(
      errors.map { case (name, ves) =>
        (name, ves.map { e => ValidationError(f(e.message, name +: e.args).getOrElse(e.message), e.args) })
      }
    )
}

object ValidationFailure {
  def of[A](errors: ValidationResult.Error*): ValidationFailure[A] = {
    ValidationFailure(errors)
  }
}