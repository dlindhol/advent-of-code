package day15

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2._

object Part1 extends IOApp {

  def solve(grid: Grid) =
    makePaths(grid).map(scorePath).compile.toList.min

  /** Make all paths - too slow for big grid */
  def makePaths(grid: Grid): Stream[Pure, Path] = {
    var bestScore = Int.MaxValue
    def go(path: Path): Stream[Pure, Path] = {
      //println(s"Path from ${path.last}")
      val score = scorePath(path)
      if (score >= bestScore) Stream.empty //bail
      else if (path.last.location == grid.destination) {
        //complete path
        if (score < bestScore) bestScore = score
        //println(s"Path Complete: $score")
        Stream.emit(path)
      } else {
        Stream.emits(nextLocations(path))
          .map(l => grid(l))   //optionally get the point from the grid
          .unNone              //keep only points in the grid
          .map(p => path :+ p) //append point to current path
          .flatMap(go)         //build up each of these paths
      }
    }
    go(Path(grid(0, 0).get))
  }

  /** Finds the points next to the end of this Path that are not already in this path. */
  def nextLocations(path: Path): List[Location] = {
    val locs = path.map(_.location) //path locations
    val (row, col) = locs.last //location of end of path
    List(
      Option.when(! locs.exists(_ == (row + 1, col)))((row + 1, col)),
      Option.when(! locs.exists(_ == (row, col + 1)))((row, col + 1)),
      Option.when(! locs.exists(_ == (row - 1, col)))((row - 1, col)),
      Option.when(! locs.exists(_ == (row, col - 1)))((row, col - 1)),
    ).unite
  }

  /** Sums the values along the path. */
  def scorePath(path: Path): Int =
    path.foldLeft(0)((s, p) => s + p.value) - path.head.value

  def run(args: List[String]): IO[ExitCode] = {
    readData(fs2.io.file.Path("data/day15test.txt"))
    //readData(fs2.io.file.Path("data/day15input.txt"))
      //.map(println)
      .compile.toVector
      .map(Grid)
      .map(solve)
      .map(println)
      .as(ExitCode.Success)
  }
}
