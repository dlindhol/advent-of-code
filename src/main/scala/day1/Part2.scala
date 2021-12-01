package day1

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.io.file._
import fs2.text

// https://adventofcode.com/2021/day/1
object Part2 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO].readAll(Path("data/day1input.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.toIntOption).unNone  //drop invalid lines
      .sliding(3)
      .map(_.toList.sum)
      .zipWithNext
      .collect {
        case (a, Some(b)) if b > a => ()
      }
      .fold(0)((n, _) => n + 1) //count
      .map(println)
      .compile.drain //.toList.map(l => println(l.length))
      .as(ExitCode.Success)
  }

}
