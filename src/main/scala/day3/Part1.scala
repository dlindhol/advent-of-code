package day3

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._
import fs2.io.file._
import fs2.text

object Part1 extends IOApp {

  val dataFile = "data/day3input.txt"

  /** Defines regex to extract data from each record. */
  val pattern = raw"[0|1]+".r

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
    00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010
    """.split("\n").toList
  )

  /**  Computes power consumption from analysis stats. */
  def computePower(stats: List[Int]): Int = {
    // For gamma, positive count maps to a 1 bit, otherwise 0
    val s: String = stats.map { n =>
      if (n > 0) '1' else '0'
    }.mkString
    val gamma = Integer.parseInt(s, 2)
    val epsilon = Math.pow(2, stats.length).toInt - 1 - gamma //complement of gamma
    gamma * epsilon
  }

  def run(args: List[String]): IO[ExitCode] = {
    data
      .through(clean)
      // Accumulate how many more '1's than '0's for each position.
      // Note that the zip will remove the extra padding.
      .fold(List.fill(100)(0)) { (acc, s) =>
        acc.zip(s).map { //string as list of Char
          case (a, '1') => a + 1
          case (a, '0') => a - 1
        }
      }
      .map(computePower)
      .map(println)
      .compile.drain
      .as(ExitCode.Success)
  }
}
