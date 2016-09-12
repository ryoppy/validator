package validator

import java.net.{Inet4Address, Inet6Address, InetAddress}

import scala.util.Try

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

  case class exactLength(y: Int) extends ValidationRule[String] {
    def name = "exactLength"

    override def args = Seq(y.toString)

    def apply(x: String) = x.length == y
  }

  case object email extends ValidationRule[String] {
    def name = "email"

    // from play framework email constraints
    private val emailRegex =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    def apply(x: String) = emailRegex.findFirstIn(x).isDefined
  }

  case object ip extends ValidationRule[String] {
    def name = "ip"

    def apply(x: String) = Try(
      InetAddress.getByName(x) match {
        case _: Inet4Address | _: Inet6Address => true
        case _ => false
      }).getOrElse(false)
  }

  case object ip4 extends ValidationRule[String] {
    def name = "ip4"

    def apply(x: String) = Try(InetAddress.getByName(x).isInstanceOf[Inet4Address]).getOrElse(false)
  }

  case object ip6 extends ValidationRule[String] {
    def name = "ip6"

    def apply(x: String) = Try(InetAddress.getByName(x).isInstanceOf[Inet6Address]).getOrElse(false)
  }

  case object url extends ValidationRule[String] {
    def name = "url"

    def apply(x: String) = Try(new java.net.URL(x)).isSuccess
  }

  case class regex(pattern: String) extends ValidationRule[String] {
    def name = "regex"

    override def args = Seq(pattern.toString)

    def apply(x: String) = pattern.r.findFirstIn(x).isDefined
  }


  // --- Ordering

  // x <= y
  case class lessThanEq[T](y: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "lessThanEq"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.lteq(x, y)
  }

  // x >= y
  case class greaterThanEq[T](y: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "greaterThanEq"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.gteq(x, y)
  }

  // x < y
  case class lessThan[T](y: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "lessThan"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.lt(x, y)
  }

  // x > y
  case class greaterThan[T](y: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "greaterThan"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.gt(x, y)
  }

  // x >= y && x <= z
  case class between[T](y: T, z: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "between"

    override def args = Seq(y.toString, z.toString)

    def apply(x: T) = ev.gteq(x, y) && ev.lteq(x, z)
  }

  case class equiv[T](y: T)(implicit ev: Ordering[T]) extends ValidationRule[T] {
    def name = "equiv"

    override def args = Seq(y.toString)

    def apply(x: T) = ev.equiv(x, y)
  }

  // I wonder if it should use Eq type class...
  case class same[A](y: A) extends ValidationRule[A] {
    def name = "same"

    override def args = Seq(y.toString)

    def apply(x: A) = x == y
  }


  // -- Date

  implicit val javaSqlDateOrdering: Ordering[java.sql.Date] =
    Ordering.fromLessThan[java.sql.Date]((x, y) => x.compareTo(y) == -1)

  implicit val jodaDateTimeOrdering: Ordering[org.joda.time.DateTime] =
    Ordering.fromLessThan[org.joda.time.DateTime](_ isBefore _)

  implicit val jodaLocalDateOrdering: Ordering[java.time.LocalDate] =
    Ordering.fromLessThan[java.time.LocalDate](_ isBefore _)

  implicit val javaLocalDateTimeOrdering: Ordering[java.time.LocalDateTime] =
    Ordering.fromLessThan[java.time.LocalDateTime](_ isBefore _)

  implicit val javaLocalDateOrdering: Ordering[java.time.LocalDate] =
    Ordering.fromLessThan[java.time.LocalDate](_ isBefore _)

  implicit val javaLocalTimeOrdering: Ordering[java.time.LocalTime] =
    Ordering.fromLessThan[java.time.LocalTime](_ isBefore _)
}
