package day15

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Part1b extends IOApp {

  def solve(grid: Grid): Int = {
    def go(ps: List[Point], point: Point): List[Point] = {
      if (point.location == grid.destination) List(point) //done
      else {
        val ps2 = visitPoint(ps, point)
        val p2 = unvisitedPointWithMinScore(ps2)
        go(ps2, p2)
      }
    }
    val p0 = grid(0, 0).get.copy(score = 0)
    go(grid.points, p0).head.score
  }

  def visitPoint(ps: List[Point], point: Point): List[Point] = {
    val (row, col) = point.location
    ps.map { p =>
      if (p == point) point.copy(unvisited = false) //mark this point visited
      else if (p.unvisited &&
        p.location == (row + 1, col) ||
        p.location == (row - 1, col) ||
        p.location == (row, col + 1) ||
        p.location == (row, col - 1)
      ) {
        val score = point.score + p.value
        if (score < p.score) p.copy(score = score)
        else p //not a better path to p
      } else p //already visited p so leave it alone
    }
  }

  def unvisitedPointWithMinScore(ps: List[Point]): Point =
    ps.filter(_.unvisited).minBy(_.score)


  def run(args: List[String]): IO[ExitCode] = {
    //readData(fs2.io.file.Path("data/day15test.txt"))
    readData(fs2.io.file.Path("data/day15input.txt"))
      //.map(println)
      .compile.toVector
      .map(Grid)
      .map(solve)
      .map(println)
      .as(ExitCode.Success)
  }
}
