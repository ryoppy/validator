package validator

trait ValidationRule[A] {
  self =>

  def name: RuleName

  def args: Seq[String] = Nil

  def apply(x: A): Boolean

  def run(x: A): Either[ValidationError, A] = {
    if (self(x)) Right(x) else Left(ValidationError(name, args))
  }

  def and(next: ValidationRule[A]): ValidationRule[A] = new ValidationRule[A] {
    override def name: RuleName = self.name

    override def apply(x: A): Boolean = self(x) && next(x)

    override def run(x: A): Either[ValidationError, A] = {
      self.run(x).left.flatMap(_ => next.run(x))
    }
  }
}

object ValidationRule {
  def apply[A](ruleName: RuleName)(f: A => Boolean): ValidationRule[A] = new ValidationRule[A] {
    def name = ruleName
    def apply(x: A): Boolean = f(x)
  }
}