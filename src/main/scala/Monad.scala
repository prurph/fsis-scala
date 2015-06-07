import simulacrum._

@typeclass trait Monad[F[_]] extends Applicative[F] { self =>

  // Lifts value into context.
  def pure[A](a: A): F[A]

  // Functors apply a function to a wrapped value (f: A => B)
  // Applicatives apply a wrapped function to a wrapped value (f: F[A => B])
  // Monads apply a function that returns a wrapped value to a wrapped value.
  // (f: A => F[B])

  // Generally in Scala flatMap and pure are kept as abstract operations, and
  // flatten is derived from them. You could alternatively use pure and flatten
  // as the abstract, and then derive flatten, but this is convenient so we can
  // delegate to the flatMap method for, say, collection types (most have legal
  // corresponding pure methods, scala.util.Try being an exception).
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // flatMap ff as first argument, must now supply an [A => B] => F[B]
  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)((f: A => B) => map(fa)(f))

  // Don't have to implement map specifically (inherits from Applicative; defined
  // in terms of pure and apply), but often see performance increase by implementing
  // in terms of flatMap and pure.
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  /* Below cannot be implemented for arbitrary Monads! */

  /* Filter for an arbitrary monad is impossible. We do not generally know what
     to do in the false case to "remove" something (although this is trivial for
     many _specific_ monads, like List, etc.)

    def filter[A, B](fa: F[A])(f: A => Boolean): F[A] =
      // What to do in the false case? For List else Nil, for Option else None,
      // but for an arbitrary monad this isn't known!
      flatMap(fa)(a => if (f(a)) pure(a) else ???)

  */


  /* Compose for two arbitrary monads is impossible. Need to know something
     about one of them.

    def compose[G[_]](implicit G: Monad[G]): Monad[Lambda[X => F[G[X]]]] =
      new Monad[Lambda[X => F[G[X]]]] {
        def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))
        // This is invalid. flatten is defined in terms of flatMap and vice versa,
        // so any attempts to use this result in a stack overflow! Unlike functors
        // and applicatives, monads cannot generally be composed together into a
        // larger Monad, although specific composite Monads can be generated. (As
        // with List monad or Option monad with other arbitrary monads). This
        // cannot be generalized to _two_ arbitrary monads, however.
        def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
          val nested = self.map(fga) { ga => G.map(ga) { a => f(a): F[G[B]] }: G[F[G[B]]] }: F[G[F[G[B]]]]
          flatten(nested) // Our "F" from flatten is F[G[_]]
        }
      }
  */
}

object Monad {
  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Option(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

}
trait MonadLaws[F[_]] {

  import IsEq._
  import Monad.ops._

  implicit val F: Monad[F]
  import F.pure

  // Result of sequential flatMaps is the same as doing the second flatMap
  // "inside" the first.
  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap { a => f(a).flatMap { b => g(b) } }

  def leftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    pure(a).flatMap(f) =?= f(a)

  def rightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap { a => pure(a) } =?= fa

}

object MonadLaws {
  def apply[F[_]](implicit F0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    val F = F0
  }
}
