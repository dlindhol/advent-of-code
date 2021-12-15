package day10

import scala.collection.mutable

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2.io.file.Path
import fs2._

object Part1 extends IOApp {

  def findIllegalCharacter(row: List[Bracket]): Option[CloseBracket] = {
    val stack: mutable.Stack[Bracket] = mutable.Stack()
    Stream.emits(row).map {
      case ob: OpenBracket  =>
        stack.push(ob)
        None
      case cb: CloseBracket =>
        if (stack.pop().inverse != cb) cb.some
        else None
    }.unNone
    .compile.toList.headOption
  }

  def score(cb: CloseBracket): Int = cb match {
    case CloseParen  => 3
    case CloseSquare => 57
    case CloseCurly  => 1197
    case ClosePoiny  => 25137
  }

  def run(args: List[String]): IO[ExitCode] = {
    //readData(Path("data/day10test.txt"))
    readData(Path("data/day10input.txt"))
      .map(findIllegalCharacter)
      .unNone
      .map(score)
      .fold(0)(_ + _)
      .map(println)
      .compile.toList
      .as(ExitCode.Success)
  }
}
