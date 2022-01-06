package day17

import cats.syntax.all._
import fs2.Stream

object Part1 extends App {

  case class YStepper(v0: Int) {
    var v = v0
    var y = 0
    def step: Int = {
      y = y + v
      v = v - 1
      y
    }
  }

  def maxY(v0: Int, targetMin: Int, targetMax: Int): Option[Int] = {
    val stepper = YStepper(v0)
    val ys: List[Int] = Stream.iterate(0)(_ => stepper.step).takeWhile(_ >= targetMin).compile.toList
    if (ys.last <= targetMax) Some(ys.max)
    else None
  }

  //val min = -10
  //val max = -5
  //println(maxY(9, min, max))

  val min = -248
  val max = -194

  /*
  Determine reasonable max v0:
    will return to y = 0 with v = -v0
    so v0 > abs(min) would forever overshoot
   */
  println(List.range(0, Math.abs(min)).map(maxY(_, min, max)).unite.max)
}
