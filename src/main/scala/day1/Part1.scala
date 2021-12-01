package day1

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.io.file._
import fs2.text

// https://adventofcode.com/2021/day/1
object Part1 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO].readAll(Path("data/day1input.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.toIntOption).unNone  //drop invalid lines
      //.filterWithPrevious((a, b) => b > a) //excludes all < current
      .zipWithNext
      .collect {
        case (a, Some(b)) if b > a => ()
      }
      .fold(0)((n, _) => n + 1) //count
      .map(println)
      .compile.drain //.toList.map(l => println(l.length))
      .as(ExitCode.Success)

    //val uri = uri"https://adventofcode.com/2021/day/1/input"
    //TODO: need cookie?
    //BlazeClientBuilder[IO](global).resource.use { client =>
    //  client.expect[String](uri).map { s =>
    //    println(s)
    //  }
    //
    //}
    //IO.unit.as(ExitCode.Success)
  }

}
