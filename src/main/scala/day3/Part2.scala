package day3

import scala.annotation.tailrec

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._
import fs2.io.file._
import fs2.text

object Part2 extends IOApp {

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

  /**
   * Breaks list into two lists with the first having the most common
   * character in the given position. Tie goes to the '1'.
   */
  def partition(ss: List[String], pos: Int): (List[String], List[String]) =
    ss.partition(_.charAt(pos) == '1') match {
      case (l1, l2) =>
        if (l1.length >= l2.length) (l1, l2)
        else (l2, l1)
    }

  def oxygen(ss: List[String]): Int = {
    @tailrec
    def go(ss: List[String], pos: Int): List[String] = ss.length match {
      case 1 => ss //done
      case _ => go(partition(ss, pos)._1, pos + 1) //TODO: what if we run out of positions?
    }
    val s = go(ss, 0).head
    Integer.parseInt(s, 2)
  }

  def co2(ss: List[String]): Int = {
    @tailrec
    def go(ss: List[String], pos: Int): List[String] = ss.length match {
      case 1 => ss //done
      case _ => go(partition(ss, pos)._2, pos + 1) //TODO: what if we run out of positions?
    }
    val s = go(ss, 0).head
    Integer.parseInt(s, 2)
  }

  def run(args: List[String]): IO[ExitCode] = {
    data
      .through(clean)
      .compile.toList
      .map { ss =>
        val o = oxygen(ss)
        val c = co2(ss)
        println(o, c, o*c)
      }
      .as(ExitCode.Success)
  }
}
