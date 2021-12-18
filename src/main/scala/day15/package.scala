import scala.util.matching.Regex

import cats.data.NonEmptyChain
import cats.effect.IO
import fs2._
import fs2.io.file._
import fs2.text

package object day15 {

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"\d+".r

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  type Data = Vector[Int]

  def parseData(s: String): Data = s.toCharArray.map(_.toString.toInt).toVector

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, Data] = _.map(parseData)

  /** Reads the data file, applies the pattern to drop invalid records, then parses data. */
  def readData(path: fs2.io.file.Path): Stream[IO, Data] =
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .through(clean)
      .through(parse)


  type Location = (Int, Int)

  case class Point(location: Location, value: Int, score: Int, unvisited: Boolean)

  type Path = NonEmptyChain[Point]
  object Path {
    def apply(point: Point, points: Point*): Path = NonEmptyChain(point, points: _*)
  }

  case class Grid(values: Vector[Vector[Int]]) {
    def apply(location: Location): Option[Point] =
      for {
        row <- values.lift(location._1)
        v <- row.lift(location._2)
      } yield Point(location, v, Int.MaxValue, true)

    /** Lower right point in grid. */
    def destination: Location = (values.length - 1, values.head.length - 1)

    def points: List[Point] = (for {
      row <- values.indices
      col <- values.head.indices
    } yield apply((row, col)).get).toList

    def shape: (Int, Int) = (values.length, values.head.length)
  }
}
