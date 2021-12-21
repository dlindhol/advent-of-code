package day13

import scala.util.matching.Regex

import cats.effect.IO
import cats.syntax.all._
import fs2._
import util.AocUtils

trait Utils extends AocUtils {

  val dot: Regex = raw"(\d+),(\d+)".r
  val fold: Regex = raw"fold along ([x|y])=(\d+)".r
  val pattern: Regex = raw"((\d+),(\d+)|fold along ([x|y])=(\d+))".r

  type Row = Either[Dot, Fold]
  type Dot = (Int, Int)
  case class Fold(dimension: Char, value: Int)

  type Data = (List[Dot], List[Fold])
  type Result = NumberOfDots
  type NumberOfDots = Int

  def parseRow(s: String): Either[Dot, Fold] = {
    s match {
      case dot(x, y) => (x.toInt, y.toInt).asLeft
      case fold(dim, v) => Fold(dim.head, v.toInt).asRight
    }
  }

  def process: Pipe[IO, Row, Data] =
    _.fold((List[Dot](), List[Fold]())) {
      case ((ds, fs), Left(dot))   => (ds :+ dot, fs)
      case ((ds, fs), Right(fold)) => (ds, fs :+ fold)
    }

  def solve: Pipe[IO, Data, Result] = _.map {
    case (ds, fs) => solve(ds, fs)
  }

  def solve(dots: List[Dot], folds: List[Fold]): NumberOfDots


  def foldDotsHorizontally(dots: List[Dot], index: Int): List[Dot] =
    dots.map { case (x, y) =>
      if (x > index) (2 * index - x, y)
      else (x, y)
    }.distinct

  def foldDotsVertically(dots: List[Dot], index: Int): List[Dot] =
    dots.map { case (x, y) =>
      if (y > index) (x, 2 * index - y)
      else (x, y)
    }.distinct
}
