package day7

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._

object Part1 extends IOApp {

  def score(list: List[Int], n: Int): Int =
    list.map(v => Math.abs(v - n)).sum

  def process: Pipe[IO, List[Int], Int] = _.map { list =>
    (0 until list.max).map { n => score(list, n) }.min
  }

  /** Stitch together the processing steps. */
  def solve(data: Stream[IO, String]): IO[Int] =
    data
      .through(clean)
      .through(parse)
      .through(process)
      .compile.toList.map(_.head)


  def run(args: List[String]): IO[ExitCode] = {
    solve(data)
      .map(println)
      .as(ExitCode.Success)
  }
}
