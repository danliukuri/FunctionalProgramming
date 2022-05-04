package streams

class IndividualTask {
  def toList(range: Seq[Int], n: Int, c: Int): List[Double] = {
    val num = 10

    def pow(x: Double ,n: Double): Double =
      if(n == 0) 1
      else if(n == 1) x
      else if(n < 0) 1 / (x * pow(x, -n-1))
      else x * pow(x, n - 1)

    def myFunc = new PartialFunction[(Int, Int), Double] {
      def apply(params: (Int, Int)): Double =
        if (params._1 > num) params._1 + params._2 else pow(params._1, n)
      def isDefinedAt(params: (Int, Int)): Boolean = params._1 != num
    }

    if (range.isEmpty) List()
    else if (!myFunc.isDefinedAt(range.head, c)) toList(range.tail, n, c)
    else myFunc(range.head, c) :: toList(range.tail, n, c)
  }

  def examplesFunctions(): Unit = {
    val list = toList(-10 to 20, 2, 5)
    println("list : " + list)
    println("list.forall(x => x % 2 == 0) : " + list.forall(x => x % 2 == 0))
    println("list.filter(x => x / 4 == 0) : " + list.filter(x => x < 10))
    println("list.sortBy(x => -x) : " + list.sortBy(x => -x))
    println("list.map(x => -x) : " + list.map(x => -x))
    println("list.reduceLeft((x1,x2) => x1 - x2)) : " + list.reduceLeft((x1,x2) => x1 - x2))
  }
}

object Main {
  def main(args: Array[String]): Unit = new IndividualTask().examplesFunctions()
}