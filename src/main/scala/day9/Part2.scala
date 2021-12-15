package day9

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text

object Part2 extends IOApp {

  def findBasins(rows: List[String]): List[Basin] = {
    //TODO: stream
    val rowsOfBasins = rows.zipWithIndex.map { case (row, i) =>
      findBasinsInRow(row, i)
    }
    rowsOfBasins.foldLeft(List[Basin]()) { (existing, basins) =>
      basins.foldLeft(existing) { (bs, b) =>
        accumulateBasin(bs, b)
      }
    }
  }

  /** Combines the given basin with pre-existing basins that overlap. */
  def accumulateBasin(basins: List[Basin], basin: Basin): List[Basin] = {
    val (matching, other) = basins.partition { b =>
      basinContainsBasin(b, basin)
    }
    val newBasin = matching.foldLeft(basin)((acc, b) => acc.addAll(b))
    newBasin :: other
  }

  /** b1 contains b2 if any point above a point in b2 is in b1. */
  def basinContainsBasin(b1: Basin, b2: Basin): Boolean = {
    b2.exists {
      case ((i, j), _) => basinContainsLocation(b1, (i - 1, j))
    }
  }

  def basinContainsLocation(basin: Basin, location: Location): Boolean =
    basin.exists {
      case (loc, _) => loc == location
    }


  /** Makes basins within the index-th row of points. */
  def findBasinsInRow(row: String, index: Int): List[Basin] =
    accumulate(List.empty, parseRow(row, index))

  /** Converts the index-th row of text input into Points. */
  def parseRow(row: String, index: Int): List[Point] =
    row.toList.zipWithIndex.map { case (c, j) =>
      ((index, j), c.toString.toInt)
    }

  /** Adds the given points to existing basins or starts a new basin. */
  def accumulate(basins: List[Basin], points: List[Point]): List[Basin] =
    //TODO or foldRight?
    points.foldLeft(basins) { (bs, p) =>
      if (p._2 == 9) bs //9s do not belong to basins
      else findBasinForPoint(bs, p) match {
        case Some(b) =>
          b += p //mutate the basin in place, eeew
          bs
        case None => Basin(p) :: bs //prepend new Basin
      }
    }

  /** Looks for a pre-existing basin for a point while processing a row. */
  def findBasinForPoint(basins: List[Basin], point: Point): Option[Basin] =
    basins.find { b =>
      // This is the basin for this point if the location left is in this basin.
      // Note, recent basins should be near the front of the list and find should short-circuit.
      val loc2: Location = (point._1._1, point._1._2 - 1)
      b.exists { p => p._1 == loc2 }
    }

  def run(args: List[String]): IO[ExitCode] = {
    //Files[IO].readAll(Path("data/day9test.txt"))
    Files[IO].readAll(Path("data/day9input.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
      .through(clean)
      //.map(println)
      .compile.toList
      .map(findBasins)
      .map { bs =>
        bs.map(_.size).sorted.takeRight(3).product
      }
      .map(println)
      .as(ExitCode.Success)
  }
}
