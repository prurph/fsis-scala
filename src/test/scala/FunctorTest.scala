import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

abstract class FunctorInstanceTest[F[_]](name: String)(implicit
  F: Functor[F],
  arbFInt: Arbitrary[F[Int]],
  eqFInt: Equal[F[Int]],
  eqFLong: Equal[F[Long]]
) extends Properties(s"Functor[$name]") {

  val laws = FunctorLaws[F]

  property("identity") = forAll { (xs: F[Int]) =>
    laws.identity(xs).isEqual
  }

  property("composition") = forAll { (xs: F[Int], f: Int => String, g: String => Long) =>
    laws.composition(xs, f, g).isEqual
  }
}

object ListFunctorTest extends FunctorInstanceTest[List]("List")
object OptionFunctorTest extends FunctorInstanceTest[Option]("Option")
