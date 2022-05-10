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

  val prop1Gen = for {
    x <- Gen.choose(11.0, 111.0)
    n <- Gen.choose(11.0, 111.0)
    c <- Gen.choose(11.0, 111.0)
  } yield (x, n, c)
  property("prop1") = forAll(prop1Gen) { ( t: (Double, Double, Double) ) =>
    val x = t._1
    val n = t._2
    val c = t._3
    (x != num) ==> lifting(x, n, c).contains(c + x)
  }

  val prop2Gen = for {
    x <- Gen.choose(-111.0, -11.0)
    n <- Gen.choose(-111.0, -11.0)
    c <- Gen.choose(-111.0, -11.0)
  } yield (x, n, c)
  property("prop2") = forAll(prop2Gen) { ( t: (Double, Double, Double) ) =>
    val x = t._1
    val n = t._2
    val c = t._3
    lifting(x, n, c).contains(pow(x, n))
  }
}