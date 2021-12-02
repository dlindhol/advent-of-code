package day2

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2._
import fs2.io.file._
import fs2.text

object Part2 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    //val steps: Stream[IO, Step] = Stream(
    //  "forward 5",
    //  "down 5",
    //  "forward 8",
    //  "up 3",
    //  "down 8",
    //  "forward 2",
    //).map(Step.apply).unNone

    Files[IO].readAll(Path("data/day2input.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(Step.apply)
      .unNone
      .fold(Position2(0, 0, 0))((p, s) => p.move(s))
      .compile.toList.map {
        case List(p @ Position2(h, d, _)) =>
          println(p)
          println(h * d)
      }
      .as(ExitCode.Success)
  }

}

case class Position2(horizontal: Int, depth: Int, aim: Int) {
  def move(step: Step): Position2 = step match {
    case Step("forward", n) => Position2(horizontal + n, depth + (aim * n), aim)
    case Step("down", n)    => Position2(horizontal, depth, aim + n)
    case Step("up", n)      => Position2(horizontal, depth, aim - n)
  }
}
