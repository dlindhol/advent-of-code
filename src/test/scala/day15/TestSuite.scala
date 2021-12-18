package day15

class TestSuite extends munit.FunSuite {

  val grid = Grid(Vector(
    Vector(0, 1, 2),
    Vector(3, 4, 5),
    Vector(6, 7, 8)
  ))
  /*
  0 1 2
  3 4 5
  6 7 8
   */

  test("paths") {
    Part1.makePaths(grid2).compile.toVector.map(Part1.scorePath).foreach(println)
  }

  test("solve1") {
    val s = Part1b.solve(grid)
    println(s)
  }

  test("replicate") {
    Part2.replicate(grid2).sortBy(_.location).foreach(println)
  }

  val grid2 = Grid(Vector(
    Vector(1, 2),
    Vector(3, 4),
  ))

  val grid3 = Grid(Vector(
    Vector(1, 9, 1, 1, 1),
    Vector(1, 9, 9, 1, 1),
    Vector(1, 9, 9, 9, 1),
    Vector(1, 9, 9, 9, 1),
    Vector(1, 9, 9, 9, 1),
  ))
}
