package day8

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._

object Part1 extends IOApp {

  /** Counts the number of display digits with 2, 3, 4, or 7 segments. */
  def simpleCount(entry: Entry): Int =
    entry.display.map(_.segmentCount).count {
      case 2 | 3 | 4 | 7 => true
      case _ => false
    }

  def process: Pipe[IO, Entry, Int] = _.fold(0) { (acc, entry) =>
    acc + simpleCount(entry)
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
