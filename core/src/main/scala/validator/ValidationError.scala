package validator

case class ValidationError(message: RuleName, args: Seq[String] = Nil)
