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
    val (d1, r1)      = extract1(digits)
    val (d4, r4)      = extract4(r1)
    val (d7, r7)      = extract7(r4)
    val (d8, r8)      = extract8(r7)
    val (d6, r6)      = extract6(d1, r8)
    val (d3, r3)      = extract3(d1, r6)
    val (d2, d5, r25) = extract25(d6, r3)
    val (d0, d9)      = extract09(d5, r25)

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

  /** 8 has 7 segments */
  def extract8(digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition(_.segmentCount == 7)
    (p._1.head, p._2)
  }

  /** 6 is the only 6-segment digit with only 1 segment in common with 1 */
  def extract6(d1: Digit, digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition { d =>
      d.segmentCount == 6 && d.segmentCountInCommon(d1) == 1
    }
    (p._1.head, p._2)
  }

  /** 3 is the only 5-segment digit with 2 segments in common with 1 */
  def extract3(d1: Digit, digits: List[Digit]): (Digit, List[Digit]) = {
    val p = digits.partition { d =>
      d.segmentCount == 5 && d.segmentCountInCommon(d1) == 2
    }
    (p._1.head, p._2)
  }

  /**
   * 2 and 5 are the remaining 5-segment digits
   * 2 has 4 segments in common with 6 (5 has 5)
   */
  def extract25(d6: Digit, digits: List[Digit]): (Digit, Digit, List[Digit]) =
    digits.partition(_.segmentCount == 5) match { //2, 5
      case (d25, r25) =>
        d25.partition(_.segmentCountInCommon(d6) == 4) match { //2
          case (d2 :: Nil, d5 :: Nil) => (d2, d5, r25)
        }
    }

  /** 0 has 4 segments in common with 5 (9 has 5) */
  def extract09(d5: Digit, digits: List[Digit]): (Digit, Digit) = {
    digits.partition(_.segmentCountInCommon(d5) == 4) match {
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

  /** Add up the display readouts */
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
