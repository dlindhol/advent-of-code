package day17

import fs2.Stream

object Part2 extends App {

  case class Stepper(vx0: Int, vy0: Int) {
    var vx = vx0
    var vy = vy0
    var x = 0
    var y = 0
    def step: (Int, Int) = {
      x = x + vx
      y = y + vy
      if (vx > 0) vx = vx - 1
      vy = vy - 1
      (x, y)
    }
  }

  def fire(vx0: Int, vy0: Int): Boolean = {
    val stepper = Stepper(vx0, vy0)
    Stream.iterate((0, 0))(_ => stepper.step)
      .takeWhile(p => p._1 <= maxX && p._2 >= minY)
      .exists { p =>
        p._1 >= minX && p._1 <= maxX &&
          p._2 >= minY && p._2 <= maxY
      }
      .compile.toList.head
  }

  // Target Area
  //val minX = 20
  //val maxX = 30
  //val minY = -10
  //val maxY = -5

  val minX = 29
  val maxX = 73
  val minY = -248
  val maxY = -194

  // Limit initial velocity search space
  val minVx = 1
  val maxVx = maxX
  val minVy = minY
  val maxVy = Math.abs(minY)

  val hits = for {
    vx0 <- minVx to maxVx
    vy0 <- minVy to maxVy
  } yield fire(vx0, vy0)
  println(hits.count(identity))
}
