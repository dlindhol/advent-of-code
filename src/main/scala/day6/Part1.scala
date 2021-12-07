package day6

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._

object Part1 extends IOApp {

  /**
   * Reduce the timer for each fish.
   * If the fish was already at 0, reset it to 6
   * and spawn a new fish at 8.
   */
  def step(fs: School): School = fs.flatMap { f =>
    //if (f.timer == 0) List(Fish(6), Fish(8))
    //else List(Fish(f.timer - 1))
    if (f == 0) List(6, 8)
    else List(f - 1)
  }

  /** Process the fish population through n steps. */
  def evolve(init: School, n: Int): Stream[IO, School] =
    Stream.range[IO, Int](0, n).fold(init) { (fs, _) => step(fs) }

  def process(steps: Int): Pipe[IO, Data, Result] =
    _.flatMap { school =>
        evolve(school, steps)
      }
      .map(_.length)

  /** Stitch together the processing steps. */
  def solve(data: Stream[IO, String], steps: Int): IO[Result] =
    data
      .through(clean)
      .through(parse)
      .through(process(steps))
      .compile.toList.map(_.head)


  def run(args: List[String]): IO[ExitCode] = {
    solve(data, 80)
      .map(println)
      .as(ExitCode.Success)
  }
}
