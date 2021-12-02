package day2

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2._
import fs2.io.file._
import fs2.text

object Part1 extends IOApp {

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
      .fold(Position(0, 0))((p, s) => p.move(s))
      .compile.toList.map {
        case List(p @ Position(h, d)) =>
          println(p)
          println(h * d)
      }
      .as(ExitCode.Success)
  }

}

case class Position(horizontal: Int, depth: Int) {
  def move(step: Step): Position = step match {
    case Step("forward", n) => Position(horizontal + n, depth)
    case Step("down", n)    => Position(horizontal, depth + n)
    case Step("up", n)      => Position(horizontal, depth - n)
  }
}

case class Step(direction: String, distance: Int)

object Step {
  def apply(s: String): Option[Step] = s.split(" ") match {
    case Array(dir, dis) => Step(dir, dis.toInt).some
    case _               => None
  }
}
