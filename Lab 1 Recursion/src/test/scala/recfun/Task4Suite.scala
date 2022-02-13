package recfun

import org.scalatest.funsuite.AnyFunSuite

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Task4Suite extends AnyFunSuite {
  import Main.task4
  test ("task4: throws an exception, x = 10") {
    intercept[IllegalArgumentException] { task4(10, 0, 0) }
  }

  test("task4: returns 12.5, x = 11.2, c = 1.3") {
    assert(task4(11.2,1.3,1) === 12.5)
  }
  test("task4: returns 9.5, x = 11.4, c = -1.9") {
    assert(task4(11.4,-1.9,1) === 9.5)
  }

  test("task4: returns 1, x = 4, n = 0") {
    assert(task4(4,1,0) === 1)
  }
  test("task4: returns 6, x = 6, n = 1") {
    assert(task4(6, 1, 1) === 6)
  }
  test("task4: returns 9, x = 3, n = 2") {
    assert(task4(3,1,2) === 9)
  }
  test("task4: returns 0.125, x = 2, c = -3") {
    assert(task4(2,1,-3) === 0.125)
  }

  test("task4: x = -4, n = -3") {
    assert(task4(-4, 1, -3) === -0.015625)
  }
  test("task4: x = 5.5, n = 5") {
    assert(task4(5.5, 1, 5) === 5032.84375)
  }
  test("task4: x = -5.5, n = 5") {
    assert(task4(-5.5, 1, 5) === -5032.84375)
  }
}