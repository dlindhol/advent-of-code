package day6

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream

class TestSuite extends munit.FunSuite {

  /** Streams test data. */
  def testData: Stream[IO, String] = Stream.emits(
    """
3,4,3,1,2
    """.split("\n").toList
  )

  test("pattern") {
    val s = pattern.findFirstIn("3,4,3,1,2")
    assertEquals(s, Some("3,4,3,1,2"))
  }

  test("solve part 1") {
    Part1.solve(testData, 80).map { r =>
      assertEquals(r, 5934L)
    }.unsafeRunSync()
  }

  test("solve part 2") {
    Part2.solve(testData, 256).map { r =>
      assertEquals(r, 26984457539L)
    }.unsafeRunSync()
  }
}
