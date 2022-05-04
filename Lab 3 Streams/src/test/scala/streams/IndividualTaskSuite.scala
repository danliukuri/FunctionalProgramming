package streams

class IndividualSuite extends munit.FunSuite {
  val task = new IndividualTask()

  test("main test") {
    assertEquals(task.toList(8 to 12, 2, 1), List[Double](64, 81, 12, 13))
  }

  test("list is empty 1") {
    assertEquals(task.toList(Seq(), 2, 1), List[Double]())
  }

  test("list is empty 2") {
    assertEquals(task.toList(10 to 10, 3, 2), List[Double]())
  }
}