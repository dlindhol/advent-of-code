package day10

import scala.collection.mutable

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import fs2._
import fs2.io.file.Path

object Part2 extends IOApp {

  def findCompletingBrackets(row: List[Bracket]): List[Bracket] = {
    val stack: mutable.Stack[Bracket] = mutable.Stack()
    val ill = Stream.emits(row).map {
      case ob: OpenBracket  =>
        stack.push(ob)
        None
      case cb: CloseBracket =>
        if (stack.pop().inverse != cb) cb.some
        else None
    }.unNone.compile.toList
    if (ill.isEmpty) {
      stack.toList.map(_.inverse)
    } else List.empty
  }

  def score(b: Bracket): Int = b match {
    case CloseParen  => 1
    case CloseSquare => 2
    case CloseCurly  => 3
    case ClosePoiny  => 4
  }

  // Got negative scores using Int!
  def score(brackets: List[Bracket]): Long =
    brackets.foldLeft(0L) { (s, b) =>
      s * 5 + score(b)
    }

  def run(args: List[String]): IO[ExitCode] = {
    //readData(Path("data/day10test.txt"))
    readData(Path("data/day10input.txt"))
      .map(findCompletingBrackets)
      .map(score)
      .filter(_ > 0)
      .compile.toList
      .map { list =>
        list.sorted.apply(list.length / 2)
      }
      .map(println)
      .as(ExitCode.Success)
  }
}
