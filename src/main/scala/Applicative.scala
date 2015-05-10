import simulacrum._

@typeclass trait Applicative[F[_]] {

  // Example: take 1 and lift into List[Int](1)
  // Called pure because it takes a raw, "pure" value that exists outside of
  // "effect system" and lifts into effect system. These are not side-effects,
  // rather that, for example, Option models the "effect" of having or not
  // having a value. List models the "effect" of having multiple values.
  def pure[A](a: A): F[A]
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)
  }
}
