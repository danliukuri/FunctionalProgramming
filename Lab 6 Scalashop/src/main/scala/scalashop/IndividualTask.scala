package scalashop

import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.pow

object IndividualTask extends App {
  def myFunc : PartialFunction[(Double, Double, Double), Double] = {
    case (x, n, c) if x > 10 => c + x
    case (x, n, c) if x < 10 => pow(x, n)
  }

  val n = 2
  val c = 10
  val future1 = Future { (-10 to 10).par.map(x => myFunc.lift(x, n, c)) }
  val future2 = Future { (-250 to 250).par.map(x => myFunc.lift(x, n, c)) }

  println(Await.result(future1, 60.seconds))
  println(Await.result(future2, 120.seconds))
}