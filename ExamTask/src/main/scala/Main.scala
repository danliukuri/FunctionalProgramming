object Main {
  def duplicateN1[A](number_of_symbols: Int, list: List[A]):List[A] =
    list.flatMap { element => List.fill(number_of_symbols)(element) }
    //list.map { element => List.fill(number_of_symbols)(element) }.flatten

  def duplicateN2[A](number_of_symbols: Int, list: List[A]):List[A] = {
    var result  = List[A]()
    for (element <- list)
      for (i <- 1 to number_of_symbols)
        result :+= element
    result
  }
  def duplicateN3[A](number_of_symbols: Int, list: List[A]):List[A] = {
    for {element <- list; i <- 1 to number_of_symbols} yield element
  }

  def main(args: Array[String]): Unit = {
    val result1 = duplicateN1(3, List('a, 'b, 'c, 'c, 'd))
    val result2 = duplicateN2(3, List('a, 'b, 'c, 'c, 'd))
    val result3 = duplicateN3(3, List('a, 'b, 'c, 'c, 'd))

      val result_to_compare = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

      println(result1)
      println(result2)
      println(result3)
      println(result_to_compare)
    }
}