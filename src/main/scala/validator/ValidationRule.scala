package validator

trait ValidationRule[A] {
  self =>

  def name: RuleName

  def apply(x: A): Boolean

  def args: Seq[String] = Nil

  def message: Option[String] = None

  def run(x: A): Either[ValidationError, A] = {
    if (self(x)) Right(x) else Left(ValidationError(message.getOrElse(name), args))
  }

  def and(next: ValidationRule[A]): ValidationRule[A] = new ValidationRule[A] {
    override def name: RuleName = next.name

    override def apply(x: A): Boolean = self(x) && next(x)

    override def args: Seq[String] = next.args

    override def message: Option[String] = next.message

    override def run(x: A): Either[ValidationError, A] = {
      self.run(x).right.flatMap(_ => next.run(x))
    }
  }

  def and(f: A => Boolean): ValidationRule[A] = and(ValidationRule(self.name)(f))

  def and(andName: RuleName)(f: A => Boolean): ValidationRule[A] = and(ValidationRule(andName)(f))

  def withMessage(msg: String): ValidationRule[A] = new ValidationRule[A] {
    override def name: RuleName = self.name

    override def apply(x: A): Boolean = self(x)

    override def args: Seq[String] = self.args

    override def message: Option[String] = Some(msg)

    override def run(x: A): Either[ValidationError, A] = {
      if (self(x)) Right(x) else Left(ValidationError(message.getOrElse(name), args))
    }
  }
}

object ValidationRule {
  def apply[A](ruleName: RuleName)(f: A => Boolean): ValidationRule[A] = new ValidationRule[A] {
    def name = ruleName

    def apply(x: A): Boolean = f(x)
  }
}