package day11

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.effect.IO
import fs2._
import util.AocUtils

trait Utils extends AocUtils {

  val pattern: Regex = raw"\d+".r
  type Row = Vector[Int]
  type Data = Grid //Vector[Vector[Int]]
  type Result = Int

  def parseRow(s: String): Vector[Int] = s.toCharArray.map(_.toString.toInt).toVector

  //make grid
  def process: Pipe[IO, Row, Data] = _.fold(Vector[Row]()) { (rows, row) =>
    rows :+ row
  }.map(Grid)

  def solve: Pipe[IO, Data, Result] = _.map(solve)

  def solve(data: Data): Result

  type Location = (Int, Int)

  def distance(loc1: Location, loc2: Location): Double =
    Math.sqrt(
      Math.pow((loc2._1 - loc1._1), 2) +
      Math.pow((loc2._2 - loc1._2), 2)
    )

  case class Point(location: Location, value: Int)

  case class Grid(values: Vector[Vector[Int]]) {
    def apply(location: Location): Option[Point] =
      for {
        row <- values.lift(location._1)
        v <- row.lift(location._2)
      } yield Point(location, v)

    def points: List[Point] = (for {
      row <- values.indices
      col <- values.head.indices
    } yield apply((row, col)).get).toList

    def shape: (Int, Int) = (values.length, values.head.length)
  }


  /** Increments each point and initiates flashing. */
  def step(points: List[Point]): List[Point] = {
    val ps = points.map { p => p.copy(value = p.value + 1) } //add one to each value
    react(ps)
  }

  /** Flashes every point > 9. */
  @tailrec
  final def react(points: List[Point]): List[Point] = {
    points.find(_.value > 9) match { //find a point ready to flash
      case Some(p) => react(flash(points, p)) //flash and recurse
      case None => points //no more flashes
    }
  }

  /** Updates neighbors of a flash and sets self to 0. */
  def flash(points: List[Point], point: Point): List[Point] = {
    val loc = point.location
    points.map { p =>
      if (p.location == point.location) p.copy(value = 0) //set the flashing point to 0
      //don't increment those that already flashed
      else if (p.value != 0 && distance(loc, p.location) < 1.5) {
        p.copy(value = p.value + 1) //react to neighbor's flash
      }
      else p
    }
  }
}
