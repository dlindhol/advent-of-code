package day5

import scala.collection.mutable
import scala.util.matching.Regex

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._
import fs2.io.file._
import fs2.text

object Part1 extends IOApp {

  val dataFile = "data/day5input.txt"

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"(\d+),(\d+) -> (\d+),(\d+)".r

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  /** Streams input data. */
  def data: Stream[IO, String] = Files[IO].readAll(Path(dataFile))
    .through(text.utf8.decode)
    .through(text.lines)

  /** Streams test data. */
  def testData: Stream[IO, String] = Stream.emits(
    """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
    """.split("\n").toList
  )

  def run(args: List[String]): IO[ExitCode] = {
    data
      .through(clean)
      .map(Line.parse)
      //.filter(l => l.isHorizontal || l.isVertical) //uncomment for part1
      // Accumulate points in a Map
      .fold(mutable.Map[Point, Int]()) { (map, line) =>
        line.points.map { p =>
          map.get(p) match {
            case Some(n) => map.addOne((p, n + 1))
            case None    => map.addOne((p, 1))
          }
        }
        map
      }
      .map(_.values.count(_ > 1)) //count how many points have multiple coverage
      .map(println)
      .compile.drain
      .as(ExitCode.Success)
  }
}

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def isHorizontal: Boolean = y1 == y2
  def isVertical:   Boolean = x1 == x2
  def points: List[Point] = {
    if (isHorizontal || isVertical)
      for {
        x <- (Math.min(x1, x2) to Math.max(x1, x2)).toList
        y <- (Math.min(y1, y2) to Math.max(y1, y2)).toList
      } yield Point(x, y)
    else { //assume 45 degrees, nx = ny, integer points
      Range.inclusive(x1, x2, Math.signum(x2 - x1).toInt)
        .zip(Range.inclusive(y1, y2, Math.signum(y2 - y1).toInt))
        .map(p => Point(p._1, p._2)).toList
    }
  }
}

object Line {
  def parse(s: String): Line = s match {
    case Part1.pattern(x1, x2, y1, y2) => Line(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
  }
}

case class Point(x: Int, y: Int)
