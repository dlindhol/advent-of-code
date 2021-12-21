package day12

import scala.util.matching.Regex

import cats.effect.IO
import cats.syntax.all._
import fs2._
import util.AocUtils

trait Utils extends AocUtils {

  val pattern: Regex = raw"(\w+)-(\w+)".r

  type Row = Connection
  type Connection = (Cave, Cave)
  type Data = CaveSystem
  type Result = NumberOfPaths
  type NumberOfPaths = Int

  def parseRow(s: String): Connection = s match {
    case pattern(c1, c2) => (Cave(c1), Cave(c2))
  }

  def process: Pipe[IO, Connection, CaveSystem] =
    _.fold(List[Connection]()) { (cs, c) =>
      c :: cs
    }.map(CaveSystem)

  def solve: Pipe[IO, Data, Result] = _.map(solve)

  def solve(data: Data): Result

  trait Cave {
    def name: String
  }
  object Cave {
    def apply(name: String): Cave = {
      if (name.head.isUpper) BigCave(name)
      else SmallCave(name)
    }
  }
  case class BigCave(name: String) extends Cave
  case class SmallCave(name: String) extends Cave

  val start: Cave = Cave("start")
  val end:   Cave = Cave("end")

  case class CaveSystem(connections: List[Connection]) {
    def neighbors(cave: Cave): List[Cave] = connections.map { case (c1, c2) =>
      if (c1 == cave) c2.some
      else if (c2 == cave) c1.some
      else None
    }.unite
  }

  type Path = List[Cave]
  object Path {
    def apply(caves: Cave*): Path = List(caves: _*)
  }
}
