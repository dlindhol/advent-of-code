package day10

class TestSuite extends munit.FunSuite {

  val testLine = "[({(<(())[]>[[{[]{<()<>>"

  test("pattern") {
    val s = pattern.findFirstIn(testLine)
    assertEquals(s, Some(testLine))
  }

}
