
import scala.util.matching.Regex

import cats.effect.IO
import fs2._
import fs2.io.file._
import fs2.text

package object day8 {
  val dataFile = "data/day8input.txt"

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"((?:\w+ ){10})\| ((?:\w+ ){3}\w+)".r

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, Entry] = _.map(parseEntry)

  def parseEntry(s: String): Entry = s match {
    case pattern(digits, display) =>
      Entry(
        digits.split(" ").toList.map(parseDigit),
        display.split(" ").toList.map(parseDigit)
      )
  }

  def parseDigit(s: String): Digit =
    Digit(List('a','b','c','d','e','f','g').map(c => s.contains(c)))

  type Data = Entry

  case class Entry(digits: List[Digit], display: List[Digit])

  case class Digit(segments: List[Boolean]) {
    def segmentCount: Int = segments.count(b => b)
    def and(digit: Digit): List[Boolean] =
      segments.zip(digit.segments).map { case (b1, b2) => b1 && b2 }
    def or(digit: Digit): List[Boolean] =
      segments.zip(digit.segments).map { case (b1, b2) => b1 || b2 }
  }

  /** Streams input data. */
  def data: Stream[IO, String] = Files[IO].readAll(Path(dataFile))
    .through(text.utf8.decode)
    .through(text.lines)

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

}
