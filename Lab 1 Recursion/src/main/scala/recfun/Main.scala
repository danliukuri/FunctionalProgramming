package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row) {
//        print(pascal(col, row) + " ")
//      }
//      println()
//    }
//
//    println(balance("(yes)".toList))
//    println(balance("(no(".toList))
//    println(balance("())(".toList))
//    println(countChange(5, List(1, 2, 5)))
//
//    println("Task4:")
//    println(task4(13,1,2))
//    println(task4(4,2,2))
//    println(task4(2,5,2))
//    println(task4(2,5,3))
//    println(task4(2,5,-2))
//    println(task4(2,5,-3))
//    println(task4(2,5,-4))
//    println(task4(-2,5,-2))
//    println(task4(-3,5,-2))
//    println(task4(-2,5,-7))
//
//    println("Task4tailrec:")
//    println(task4Tailrec(13,1,2))
//    println(task4Tailrec(4,2,2))
//    println(task4Tailrec(2,5,2))
//    println(task4Tailrec(2,5,3))
//    println(task4Tailrec(2,5,-2))
//    println(task4Tailrec(2,5,-3))
//    println(task4Tailrec(2,5,-4))
//    println(task4Tailrec(-2,5,-2))
//    println(task4Tailrec(-3,5,-2))
//    println(task4Tailrec(-2,5,-7))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def check(score: Int, chars: List[Char]): Int = {
      if (chars.isEmpty || score < 0) score
      else if (chars.head == '(') check(score + 1, chars.tail)
      else if (chars.head == ')') check(score - 1, chars.tail)
      else check(score, chars.tail)
    }

    0 == check(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  /**
   * Exercise 4
   * Variant 25
   */
  def task4(x: Double, c: Double, n: Double): Double = {
    require(x != 10, "Values are invalid")
    def pow(n: Double): Double = {
      if(n == 0) 1
      else if(n == 1) x
      else if(n < 0) 1 / (x * pow(-n-1))
      else x * pow(n - 1)
    }

    if(x > 10) c + x
    else pow(n)
  }

  def task4Tailrec(x: Double, c: Double, n: Double): Double = {
    require(x != 10, "Values are invalid")
    @tailrec
    def pow(v: Double , n: Double): Double = {
      if(n == 0) 1
      else if(n == 1) v
      else if (n == -1) v / x / x // Because we waste 2 iterations on the start
      else if(n < 0) pow(v / x, n + 1)
      else pow(x * v, n - 1)
    }
    if(x > 10) c + x
    else pow(x, n)
  }
}