package day14

import scala.util.matching.Regex

import cats.effect.IO
import cats.syntax.all._
import fs2._
import util.AocUtils

trait Utils extends AocUtils {

  val template: Regex = raw"(\w+)".r
  val insertion: Regex = raw"(\w\w) -> (\w)".r
  val pattern: Regex = raw"(\w\w -> \w|\w+)".r

  type Polymer = String
  type Insertion = (String, Char)

  type Row = Either[Polymer, Insertion]

  type Data = (Polymer, Map[String, Char])
  type Result = Long

  def parseRow(s: String): Either[Polymer, Insertion] = {
    //println(s)
    s match {
      case insertion(pair, c) => (pair, c.head).asRight
      case template(s) => s.asLeft
    }
  }

  def process: Pipe[IO, Row, Data] =
    _.fold(("", List[Insertion]())) {
      case ((_, is), Left(t)) => (t, is)
      case ((t, is), Right(i)) => (t, is :+ i)
    }.map { case (t, is) =>
      (t, is.toMap)
    }

  def solve: Pipe[IO, Data, Result] = _.map {
    case (template, rules) => solve(template, rules)
  }

  def solve(template: Polymer, rules: Map[String, Char]): Long



}
