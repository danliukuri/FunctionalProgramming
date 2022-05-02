package objsets

class PostListSuite extends munit.FunSuite {

  val emptyList: PostList = Nil

  test("head: on empty list") {
    val exception = intercept[java.util.NoSuchElementException] { emptyList.head }
    assertEquals(exception.getMessage, "head of EmptyList")
  }

  test("tail: on empty list") {
    val exception = intercept[java.util.NoSuchElementException] { emptyList.tail }
    assertEquals(exception.getMessage, "tail of EmptyList")
  }

  test("isEmpty: on empty list") {
    assert(emptyList.isEmpty)
  }

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds
}