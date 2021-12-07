package day7

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream

class TestSuite extends munit.FunSuite {

  /** Streams test data. */
  def testData: Stream[IO, String] = Stream.emits(
    """
16,1,2,0,4,2,7,1,2,14
    """.split("\n").toList
  )

  test("pattern") {
    val s = pattern.findFirstIn("16,1,2,0,4,2,7,1,2,14")
    assertEquals(s, Some("16,1,2,0,4,2,7,1,2,14"))
  }

  test("parse") {
    testData.through(clean).through(parse).compile.toList.map { lists =>
      assertEquals(lists.head, List(16,1,2,0,4,2,7,1,2,14))
    }.unsafeRunSync()
  }

  test("solve part 1") {
    Part1.solve(testData).map { r =>
      assertEquals(r, 37)
    }.unsafeRunSync()
  }

  test("solve part 2") {
    Part2.solve(testData).map { r =>
      assertEquals(r, 168)
    }.unsafeRunSync()
  }
}
