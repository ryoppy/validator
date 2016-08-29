package validator

trait ValidationRules {

  case class minLength(y: Int) extends ValidationRule[String] {
    def name = "minLength"

    override def args = Seq(y.toString)

    def apply(x: String) = x.length >= y
  }

  case class maxLength(y: Int) extends ValidationRule[String] {
    def name = "maxLength"

    override def args = Seq(y.toString)

    def apply(x: String) = x.length <= y
  }

  case class min[T: Numeric](y: T) extends ValidationRule[T] {
    def name = "min"

    override def args = Seq(y.toString)

    def apply(x: T) = implicitly[Numeric[T]].gt(x, y)
  }

  case class max[T: Numeric](y: T) extends ValidationRule[T] {
    def name = "max"

    override def args = Seq(y.toString)

    def apply(x: T) = implicitly[Numeric[T]].lt(x, y)
  }

  case class equiv[T: Numeric](y: T) extends ValidationRule[T] {
    def name = "eq"

    override def args = Seq(y.toString)

    def apply(x: T) = implicitly[Numeric[T]].equiv(x, y)
  }

}
