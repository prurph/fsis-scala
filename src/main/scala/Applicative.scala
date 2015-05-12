import simulacrum._

@typeclass trait Applicative[F[_]] extends Functor[F] { self =>

  // Example: take 1 and lift into List[Int](1)
  // Called pure because it takes a raw, "pure" value that exists outside of
  // "effect system" and lifts into effect system. These are not side-effects,
  // rather that, for example, Option models the "effect" of having or not
  // having a value. List models the "effect" of having multiple values.
  def pure[A](a: A): F[A]

  // Takes two proper types, A and B, and an F[A], but instead of taking A => B,
  // as with Functor's map, takes a type that exists _within_ the type
  // constructor. Applicative operates inside the "container", Functor "unwraps"
  // and "rewraps".
  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  /* Derived methods */

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a,b))))

  // Map is just apply but with function not wrapped in type constructor F.
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_,b))) // or: apply(fb)(map(fa)(f.curried))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => f(_, b, c))) // or: apply(fa)(map2(fb, fc)(a => f(a, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c,d)) => f(a, b, c, d) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c))

  // Like lift from functor.
  def flip[A, B](ff: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(ff)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
        // We have F[G[A]], so if we had a F[G[A] => G[B]], we could simply use
        // F's apply with that function. Thus we unpack ff to give us a function
        // gab: G[A => B] and call G.flip on it to yield G[A] => G[B]
        val x: F[G[A] => G[B]] = self.map(ff)(G.flip)
        self.apply(fga)(x)
      }
    }
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)

    def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (None, _) => None
      case (Some(a), None) => None
      case (Some(a), Some(f)) => Some(f(a))
    }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)
    def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] = for {
      a <- fa
      f <- ff
    } yield f(a)
  }
}
