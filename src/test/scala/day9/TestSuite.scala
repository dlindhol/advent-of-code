package day9

class TestSuite extends munit.FunSuite {

  val testLine = "2199943210"

  test("pattern") {
    val s = pattern.findFirstIn(testLine)
    assertEquals(s, Some(testLine))
  }

  test("parse") {
    assertEquals(parseData(testLine), Vector(2,1,9,9,9,4,3,2,1,0))
  }

  test("parseRow") {
    assertEquals(Part2.parseRow(testLine, 0).last, ((0,9),0))
  }

  test("initial basins") {
    val bs = Part2.findBasinsInRow(testLine, 0)
    assertEquals(bs.length, 2)
    assertEquals(bs(0).size, 5) //last basin first
    assertEquals(bs(1).size, 2)
  }

  test("basins at end") {
    val bs = Part2.findBasinsInRow("19191", 0)
    assertEquals(bs.length, 3)
    assertEquals(bs(0).size, 1)
    assertEquals(bs(1).size, 1)
    assertEquals(bs(2).size, 1)
  }

  test("basinContainsBasin") {
    val upper = Part2.findBasinsInRow("991199", 0).head
    val lower = Part2.findBasinsInRow("911999", 1).head
    assert(Part2.basinContainsBasin(upper, lower))
  }
}
