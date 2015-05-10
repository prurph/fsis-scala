import simulacrum._

// A functor is a type class that abstracts over type constructors (F[_]) that
// can define a map function.
// @typeclass from simulacrum gives ops, implicit summoning.
@typeclass trait Functor[F[_]] { self =>

  // Get F from Functor; A and B from type parameters to map
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /* Derived operations off of Functor */

  // Lift the pure function A => B to a function executing with values wrapped
  // in the functor's context.
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  // Take the type constructor and map the constant function over fa. Ignores
  // the input to map and just returns b.
  // Functor[[List]].as(List(1,2), "foo") = List("foo", "foo")
  def as[A, B](fa: F[A], b: => B): F[B] =
    map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] =
    as(fa, ())

  // Takes a type constructor, G (required to be a functor by the implicit
  // parameter). Returns Functor[F[G[?]]], which must be expressed using the
  // Lambda notation in order to compile. Note Lambda[X => F[G[X]]] acts as a
  // type constructor with a single "variable" X, thus satisfying the typing of
  // Functor as F[_].
  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        // Need to map over outer F, accessing G[A], which we can then map over.
        // Thus introduce reference to Functor for F as self above.
        self.map(fga)(ga => G.map(ga)(a => f(a)))
    }
}

// Laws that a Functor's #map must obey
trait FunctorLaws {

  // For some F[A] as input, getting an instance of a functor for F, and calling
  // its map on that input with the identity function as input must result in
  // that input.
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) =
    F.map(fa)(a => a) == fa

  // Can either map piecewise (map f over fa to give F[B] then map g over that
  // to give F[C]) or hook f and up together to yield new function A => C and
  // then map that directly over fa to give F[C].
  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

// Define implicit Functors for common type constructors. Access (as in REPL)
// with implicitly[Functor[List]], etc.
object Functor {

  // Parameterize Functor by List datatype (Functor requires type constructor as
  // argument, List is a type constructor)
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // This must be def because we require the type parameter A, and vals cannot
  // have type parameters. X => ? is a type constructor that when you apply a
  // proper type A, you get back a function X => A.
  // We cannot do Function1[X,B], because that's a proper type, whereas
  // Functors need type constructors of one argument (F[_] from the trait).
  // Likewise, can't just do Functor[Function1] as Function1 is a binary type
  // constructor: it needs input AND output type.
  // [X => ?] is sugar from kind-projector for:
  // [Lambda[X => Function1[X, ?]]], which is more kind-projector sugar for:
  // [({type l[a] = Function1[X,a]})#l], where a = ?
  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    def map[A, B](fa: X => A)(f: A => B): X => B = fa andThen f
  }
}
