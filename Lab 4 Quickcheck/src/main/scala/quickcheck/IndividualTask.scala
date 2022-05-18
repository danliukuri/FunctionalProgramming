package quickcheck
import org.scalacheck._
import Prop._
import scala.math.pow

abstract class IndividualTask extends Properties("Individual Task") {
  val num = 10

  def myFunc : PartialFunction[(Double, Double, Double), Double] = {
    case (x, n, c)
      if x > num => c + x
    case (x, n, c)
      if x < num => pow(x, n)
  }

  def lifting(x: Double, n: Double, c : Double): Option[Double] = {
    myFunc.lift(x, n, c)
  }

  val propGen: Gen[(Double, Double, Double)] = for {
    x <- Gen.choose(1.0, 19.0)
    n <- Gen.choose(1.0, 19.0)
    c <- Gen.choose(1.0, 19.0)
  } yield (x, n, c)

  property("prop1") = forAll(propGen) { ( t: (Double, Double, Double) ) =>
    val (x ,n, c) = t
    (x > num) ==> lifting(x, n, c).contains(c + x)
  }
  property("prop2") = forAll(propGen) { ( t: (Double, Double, Double) ) =>
    val (x ,n, c) = t
    (x < num) ==> lifting(x, n, c).contains(pow(x, n))
  }
}