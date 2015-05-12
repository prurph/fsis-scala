trait Equal[A] {
  def isEqual(lhs: A, rhs: A): Boolean
}

object Equal {
  def instance[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def isEqual(lhs: A, rhs: A) = f(lhs, rhs)
  }

  def natural[A]: Equal[A] = instance[A](_ == _)

  implicit val eqInt: Equal[Int] = natural
  implicit val eqString: Equal[String] = natural
  implicit val eqLong: Equal[Long] = natural
  implicit def eqList[A](implicit EA: Equal[A]): Equal[List[A]] = instance((x, y) =>
    x.size == y.size && {
      x.zip(y).forall { case (xx, yy) => EA.isEqual(xx, yy) }
    }
  )
  implicit def optionEqual[A](implicit EA: Equal[A]): Equal[Option[A]] = Equal.instance((x, y) => (x, y) match {
    case (Some(x), Some(y)) => EA.isEqual(x, y)
    case (None, None) => true
    case _ => false
  })
}

case class IsEq[A](lhs: A, rhs: A) {
  def isEqual(implicit eq: Equal[A]): Boolean = eq.isEqual(lhs, rhs)
}

object IsEq {

  implicit class Syntax[A](lhs: A) {
    def =?=(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }
}
