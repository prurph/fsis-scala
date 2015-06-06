import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

// Everything passed in in the second set of parens is implicit.
abstract class MonadInstanceTest[F[_]](name: String)(implicit
  F: Monad[F],
  arbFInt: Arbitrary[F[Int]],
  arbFLong: Arbitrary[F[Long]],
  arbFString: Arbitrary[F[String]],
  eqFInt: Equal[F[Int]],
  eqFString: Equal[F[String]],
  eqFLong: Equal[F[Long]]
) extends Properties(s"Monad[$name]") {

  val laws = MonadLaws[F]

  property("flatMap associativity") = forAll { (fa: F[Int], f: Int => F[String], g: String => F[Long]) =>
    laws.flatMapAssociativity(fa, f, g).isEqual
  }

  property("monad left identity") = forAll { (a: Int, f: Int => F[String]) =>
    laws.leftIdentity(a, f).isEqual
  }

  property("monad right identity") = forAll { (fa: F[Int]) =>
    laws.rightIdentity(fa).isEqual
  }
}

object ListMonadTest extends MonadInstanceTest[List]("List")
object OptionMonadTest extends MonadInstanceTest[Option]("Option")
