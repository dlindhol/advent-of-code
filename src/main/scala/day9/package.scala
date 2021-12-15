import scala.collection.mutable
import scala.util.matching.Regex

import cats.effect.IO
import cats.syntax.all._
import fs2._
import fs2.io.file._
import fs2.io.file.Path
import fs2.text

package object day9 {

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"\d+".r

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  //TODO: try streaming, or Grid as Data
  type Row = Vector[Int]
  type Data = Row
  type Location = (Int, Int)

  type Point = (Location, Int) //location and value
  type Basin = mutable.Set[Point]
  object Basin {
    def apply(p: Point*): Basin = mutable.Set(p: _*)
  }

  def parseData(s: String): Row = s.toCharArray.map(_.toString.toInt).toVector

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, Data] = _.map(parseData)

  /** Reads the data file, applies the pattern to drop invalid records, then parses data. */
  def readData(path: Path): Stream[IO, Data] =
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .through(clean)
      .through(parse)


  case class Grid(rows: Vector[Vector[Int]]) {
    def apply(location: Location): Option[Int] =
      for {
        row <- rows.lift(location._1)
        v   <- row.lift(location._2)
      } yield v

    def nrow: Int = rows.length
    def ncol: Int = rows(0).length

    def getAdjacents(i: Int, j: Int): Vector[Int] = {
      Vector(
        apply(i - 1, j),
        apply(i + 1, j),
        apply(i, j - 1),
        apply(i, j + 1)
      ).unite //off-edge points dropped
    }
  }

  def isMinima(grid: Grid, i: Int, j: Int): Boolean =
    grid(i, j).exists { v =>
      grid.getAdjacents(i, j).forall(_ > v)
    }

  def findMinimaLocations(grid: Grid): Vector[Location] =
    (for {
      i <- 0 until grid.nrow
      j <- 0 until grid.ncol
    } yield (i, j)).toVector.filter {
      case (i, j) => isMinima(grid, i, j)
    }

}
