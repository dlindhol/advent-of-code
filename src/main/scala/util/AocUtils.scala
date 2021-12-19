package util

import scala.util.matching.Regex

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2._
import fs2.io.file._
import fs2.text

trait AocUtils {

  /** Defines regex to extract data from each record. */
  val pattern: Regex

  /** Defines type to represent a row in input data. */
  type Row

  /** Defines result type of processing. */
  type Data

  /** Defines the type of the solution. */
  type Result

  /** Parses a row of input data. */
  def parseRow(s: String): Row

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, Row] = _.map(parseRow)

  /** Converts a stream of rows into a stream of data. */
  def process: Pipe[IO, Row, Data]

  /** Converts a stream of data into the final result. */
  def solve: Pipe[IO, Data, Result]

  /** Reads the data file, applies the pattern to drop invalid records, then parses data. */
  def read(path: Path): Stream[IO, String] =
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .through(clean)

  /** Reads the data file, applies the pattern to drop invalid records, then parses data. */
  def run(path: Path): Unit =
    read(path)
      .through(parse)
      .through(process)
      .through(solve)
      .map(println)
      .compile.drain.unsafeRunSync()

}
