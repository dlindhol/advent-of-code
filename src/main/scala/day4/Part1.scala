package day4

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._
import fs2.io.file._
import fs2.text

object Part1 extends IOApp {

  val dataFile = "data/day4input.txt"

  /** Defines regex to extract data from each record. */
  val pattern = raw".*\d+.*".r //keep only rows with numbers

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
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
    """.split("\n").toList
  )

  def parseDraws(s: String): List[Int] =
    s.split(",").toList.map(_.toInt)

  def parseBoard(rows: List[String]): Board =
    Board(rows.flatMap(_.trim.split(raw"\s+")).map(_.toInt).map(UnmarkedNumber))

  def run(args: List[String]): IO[ExitCode] = {
    data
      .through(clean)
      //.map(println)
      .compile.toList
      .map { list =>
        val draws = parseDraws(list.head)
        //TODO: avoid mutable boards
        var boards = list.tail.grouped(5).toList.map(parseBoard)
        // Apply each draw to all the boards.
        // Report winning boards then remove them.
        // The first score is the answer to part1.
        // The last score is the answer to part2.
        draws.map { draw =>
          boards = boards.map(_.mark(draw)).filter { b =>
            if (b.isWinner) {
              val score = b.numbers.collect {
                case UnmarkedNumber(n) => n
              }.sum * draw
              println(score)
              false
            } else true
          }
        }
      }
      .as(ExitCode.Success)
  }
}

sealed trait Number { def value: Int }
final case class MarkedNumber(value: Int) extends Number
final case class UnmarkedNumber(value: Int) extends Number

case class Board(numbers: List[Number]) {
  /** Makes a new Board with the matching number marked. */
  def mark(draw: Int): Board = Board(numbers.map { n =>
    if (n.value == draw) MarkedNumber(draw) else n
  })

  /** Presents the Board as a list of rows of Numbers. */
  def rows: List[List[Number]] = List.range(0, 5).map { i =>
    List.range(0, 5).map(j => numbers(i * 5 + j))
  }

  /** Presents the Board as a list of columns of Numbers. */
  def cols: List[List[Number]] = List.range(0, 5).map { i =>
    List.range(0, 5).map(j => numbers(j * 5 + i))
  }

  /** Determines if this Board has a completely marked row or column. */
  def isWinner: Boolean =
    rows.exists(_.forall(_.isInstanceOf[MarkedNumber])) ||
    cols.exists(_.forall(_.isInstanceOf[MarkedNumber]))
}
