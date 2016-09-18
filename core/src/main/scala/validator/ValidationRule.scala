package validator

trait ValidationRule[A] {
  self =>

  def name: RuleName

  def apply(x: A): Boolean

  def args: Seq[String] = Nil

  def message: Option[String] = None

  def run(x: A): Either[Set[ValidationError], A] = {
    if (self(x)) Right(x) else Left(Set(ValidationError(message.getOrElse(name), args)))
  }

  def and(that: ValidationRule[A]): ValidationRule[A] = new ValidationRule[A] {
    def name: RuleName = that.name

    def apply(x: A): Boolean = self(x) && that(x)

    override def args: Seq[String] = that.args

    override def message: Option[String] = that.message

    override def run(x: A): Either[Set[ValidationError], A] = {
      (self.run(x), that.run(x)) match {
        case (Right(_), Right(_)) => Right(x)
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e1), _) => Left(e1)
        case (_, Left(e2)) => Left(e2)
      }
    }
  }

  def and(f: A => Boolean): ValidationRule[A] = and(ValidationRule(self.name)(f))

  def and(andName: RuleName)(f: A => Boolean): ValidationRule[A] = and(ValidationRule(andName)(f))

  def withMessage(msg: String): ValidationRule[A] = new ValidationRule[A] {
    def name: RuleName = self.name

    def apply(x: A): Boolean = self(x)

    override def args: Seq[String] = self.args

    override def message: Option[String] = Some(msg)

    override def run(x: A): Either[Set[ValidationError], A] = {
      if (self(x)) Right(x) else Left(Set(ValidationError(message.getOrElse(name), args)))
    }
  }
}

object ValidationRule {
  def apply[A](ruleName: RuleName)(f: A => Boolean): ValidationRule[A] = new ValidationRule[A] {
    def name = ruleName

    def apply(x: A): Boolean = f(x)
  }
}