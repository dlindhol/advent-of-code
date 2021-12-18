package day15

import scala.annotation.tailrec

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._

object Part2 extends IOApp {

  /** Recursively visits all points until it gets to the destination. */
  def solve(grid: Grid): Int = {
    val points = replicate(grid) //use grid.points to replicate Part 1
    val destination = points.last.location
    val p0 = points.head.copy(score = 0)

    @tailrec
    def go(ps: List[Point], point: Point): List[Point] = {
      if (point.location._1 == point.location._2) println(point, ps.length)
      if (point.location == destination) List(point) //done
      else {
        val ps2 = visitPoint(ps, point)
        val p2 = unvisitedPointWithMinScore(ps2)
        go(ps2, p2)
      }
    }

    go(points, p0).head.score
  }

  /** Updates best scores of a point's neighbors and removes itself from list of unvisited. */
  def visitPoint(ps: List[Point], point: Point): List[Point] = {
    val (row, col) = point.location
    ps.map { p =>
      if (p == point) None //drop this point instead of using unvisited property
      else if (
        p.location == (row + 1, col) ||
        p.location == (row - 1, col) ||
        p.location == (row, col + 1) ||
        p.location == (row, col - 1)
      ) {
        val score = point.score + p.value
        if (score < p.score) p.copy(score = score).some
        else p.some //not a better path to p
      } else p.some //already visited p so leave it alone
    }.unite
  }

  /** Finds the next point to visit. */
  def unvisitedPointWithMinScore(ps: List[Point]): Point =
    ps.filter(_.score < Int.MaxValue).minBy(_.score)

  /** Replicates initial Grid 25 times per Part 2 instructions. */
  def replicate(grid: Grid): List[Point] = {
    val (nrow, ncol) = grid.shape
    val tiles = for {
      i <- 0 until 5
      j <- 0 until 5
    } yield (i, j)
    tiles.toList.flatMap { case (i, j) =>
      grid.points.map { p =>
        val v = p.value + i + j match {
          case n if n > 9 => n % 9
          case n => n
        }
        val row = p.location._1 + i * nrow
        val col = p.location._2 + j * ncol
        p.copy(location = (row, col), value = v)
      }
    }
  }

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
