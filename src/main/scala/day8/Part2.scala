package day8

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._

object Part2 extends IOApp {

  /** Ids the digits and returns them in order */
  def idDigits(e: Entry): List[Digit] = {
    idDigits(e.digits)
  }

  def idDigits(digits: List[Digit]): List[Digit] = {
    val (d1, r1) = extract1(digits)
    val (d4, r4) = extract4(r1)
    val (d7, r7) = extract7(r4)
    val (d8, r8) = extract8(r7)
    val (d6, r6) = extract6(d1, r8)
    val (d2, d3, d5, r235) = extract235(d1, d6, r6)
    val (d0, d9) = extract09(d5, r235)

    List(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)
  }

  /** 1 has 2 segments */
  def extract1(digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition(_.segmentCount == 2)
    (p._1.head, p._2)
  }

  /** 4 has 4 segments */
  def extract4(digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition(_.segmentCount == 4)
    (p._1.head, p._2)
  }

  /** 7 has 3 segments */
  def extract7(digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition(_.segmentCount == 3)
    (p._1.head, p._2)
  }

  /** 8 has all seven segments */
  def extract8(digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition(_.segmentCount == 7)
    (p._1.head, p._2)
  }

  /** 6 is a 6-segment digit that shares only one segment with 1 */
  def extract6(d1: Digit, digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition { d =>
      d.segmentCount == 6 && d.and(d1).count(identity) == 1
        //v1.segments.zip(d.segments).count { case (b1, b2) => b1 && b2 } == 1
    }
    (p._1.head, p._2)
  }

  /**
   * 2, 3, and 5 are the 5-segment digits
   * 3 has both 1 segments
   * 5 shares a missing segment with 6
   */
  def extract235(d1: Digit, d6: Digit, digits: List[Digit]): (Digit, Digit, Digit, List[Digit]) = {
    digits.partition(_.segmentCount == 5) match {
      case (d235, r235) =>
        d235.partition { d =>
          d.and(d1).count(identity) == 2 //3
        } match {
          case (d3 :: Nil, d25) =>
            d25.partition { d =>
              d.or(d6).count(identity)  == 7  //2
            } match {
              case (d2 :: Nil, d5 :: Nil) => (d2, d3, d5, r235)
            }
        }
    }
  }

  /** 0 has 4 segments in common with 5 (9 has 5) */
  def extract09(d5: Digit, digits: List[Digit]): (Digit, Digit) = {
    digits.partition { d =>
      d.and(d5).count(identity) == 4
    } match {
      case (d0 :: Nil, d9 :: Nil) => (d0, d9)
    }
  }

  /** Interpret the digits and apply them to the display. */
  def readDisplay(e: Entry): Int = {
    val n = e.display.length //number of display digits
    val digits = idDigits(e)
    (0 until n).map { i =>
      val d = e.display(i)
      digits.indexOf(d) * Math.pow(10, n - 1 - i).toInt
    }.sum
  }

  def process: Pipe[IO, Entry, Int] = _.fold(0) { (acc, entry) =>
    acc + readDisplay(entry)
  }

  /** Stitch together the processing steps. */
  def solve(data: Stream[IO, String]): IO[Int] =
    data
      .through(clean)
      .through(parse)
      .through(process)
      .compile.toList.map(_.head)


  def run(args: List[String]): IO[ExitCode] = {
    solve(data)
      .map(println)
      .as(ExitCode.Success)
  }
}
