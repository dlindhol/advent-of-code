package day11

import fs2.io.file.Path

object Part1 extends App with Utils {

  def solve(grid: Grid): Int = {
    var totalFlashes = 0
    (0 until 100).foldLeft(grid.points){ (ps, i) =>
      val ps2 = step(ps)
      totalFlashes += ps2.count(_.value == 0)
      ps2
    }
    totalFlashes
  }

  //run(Path("data/day11test.txt"))
  run(Path("data/day11input.txt"))
}
