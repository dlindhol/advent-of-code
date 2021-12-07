import scala.util.matching.Regex

import cats.effect.IO
import fs2._
import fs2.io.file._
import fs2.text

/** Domain model */
package object day7 {

  val dataFile = "data/day7input.txt"

  /** Streams input data. */
  def data: Stream[IO, String] = Files[IO].readAll(Path(dataFile))
    .through(text.utf8.decode)
    .through(text.lines)

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"(\d+(?:,\d+)+)".r

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, List[Int]] = _.map { s =>
    s.split(",").toList.map(_.toInt)
  }
}
