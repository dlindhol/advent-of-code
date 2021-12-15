package day9

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2.io.file.Path

object Part1 extends IOApp {

  def solve(grid: Grid): Int = {
    findMinimaLocations(grid)
      .map(grid.apply).unite
      .map(_ + 1).sum //add up risks
  }

  def run(args: List[String]): IO[ExitCode] = {
    //readData(Path("data/day9test.txt"))
    readData(Path("data/day9input.txt"))
      //.map(println)
      .compile.toVector
      .map(Grid)
      .map(solve)
      .map(println)
      .as(ExitCode.Success)
  }
}
