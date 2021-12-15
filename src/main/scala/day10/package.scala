import scala.util.matching.Regex

import cats.effect.IO
import cats.syntax.all._
import fs2._
import fs2.io.file._
import fs2.io.file.Path
import fs2.text

package object day10 {

  /** Defines regex to extract data from each record. */
  val pattern: Regex = raw"[\[\](){}<>]+".r

  /** Defines Pipe to preserve only valid data values. */
  val clean: Pipe[IO, String, String] = ss =>
    ss.map(s => pattern.findFirstIn(s)).unNone

  type Data = List[Bracket]

  def parseData(s: String): Data = s.toList.map(Bracket.apply)

  /** Converts raw data into domain data. */
  val parse: Pipe[IO, String, Data] = _.map(parseData)

  /** Reads the data file, applies the pattern to drop invalid records, then parses data. */
  def readData(path: Path): Stream[IO, Data] =
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .through(clean)
      .through(parse)


  trait Bracket {
    def inverse: Bracket
  }
  trait OpenBracket extends Bracket
  trait CloseBracket extends Bracket
  case object OpenParen   extends OpenBracket {
    def inverse = CloseParen
  }
  case object OpenSquare  extends OpenBracket {
    def inverse = CloseSquare
  }
  case object OpenCurly   extends OpenBracket {
    def inverse = CloseCurly
  }
  case object OpenPoiny   extends OpenBracket {
    def inverse = ClosePoiny
  }
  case object CloseParen  extends CloseBracket {
    def inverse = OpenParen
  }
  case object CloseSquare extends CloseBracket {
    def inverse = OpenSquare
  }
  case object CloseCurly  extends CloseBracket {
    def inverse = OpenCurly
  }
  case object ClosePoiny  extends CloseBracket {
    def inverse = OpenPoiny
  }

  object Bracket {
    def apply(char: Char): Bracket = char match {
      case '(' => OpenParen
      case '[' => OpenSquare
      case '{' => OpenCurly
      case '<' => OpenPoiny
      case ')' => CloseParen
      case ']' => CloseSquare
      case '}' => CloseCurly
      case '>' => ClosePoiny
    }
  }
}
