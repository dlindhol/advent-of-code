package day11

import fs2.io.file.Path

object Part2 extends App with Utils {

  def solve(grid: Grid): Int = {
    (0 until 500).foldLeft(grid.points){ (ps, i) =>
      val ps2 = step(ps)
      val n = ps2.count(_.value == 0)
      if (n == ps2.length) return i+1 //short-circuit with solution when all points flash
      ps2
    }
    -1 //No step where all points flash was found
  }

  //run(Path("data/day11test.txt"))
  run(Path("data/day11input.txt"))
}
