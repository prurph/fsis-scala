import simulacrum._

@typeclass trait Monad[F[_]] extends Applicative[F] { self =>

  // Lifts value into context.
  def pure[A](a: A): F[A]

  // Functors apply a function to a wrapped value (f: A => B)
  // Applicatives apply a wrapped function to a wrapped value (f: F[A => B])
  // Monads apply a function that returns a wrapped value to a wrapped value.
  // (f: A => F[B])
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
}
