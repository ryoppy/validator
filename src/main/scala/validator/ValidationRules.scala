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

  case class min[T](y: T)(implicit ev: Numeric[T]) extends ValidationRule[T] {
    def name = "min"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.gteq(x, y)
  }

  case class max[T](y: T)(implicit ev: Numeric[T]) extends ValidationRule[T] {
    def name = "max"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.lteq(x, y)
  }

  case class equiv[T](y: T)(implicit ev: Numeric[T]) extends ValidationRule[T] {
    def name = "equiv"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.equiv(x, y)
  }

  case class equal(y: String) extends ValidationRule[String] {
    def name = "equal"

    override def args = Seq(y)

    def apply(x: String) = x == y
  }

  case object email extends ValidationRule[String] {
    def name = "email"

    // from play framework email constraints
    private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    def apply(x: String) = emailRegex.findFirstIn(x).isDefined
  }

  // I wonder if it should use Eq type class...
  case class same[A](y: A) extends ValidationRule[A] {
    def name = "same"

    override def args = Seq(y.toString)

    def apply(x: A) = x == y
  }
}
