package day14

import scala.collection.mutable

object Part2 extends App with Utils {

  val depth = 40
  //Still too slow

  /** Recurses but only accumulates counts when it hits the target depth. */
  def solve(template: Polymer, rules: Map[String, Char]): Long = {
    val acc = mutable.Map[Char, Long]()
    def go(pair: String, depth: Int): Unit = {
      //println(depth, pair)
      if (depth == 0) count(acc, pair.head) //count the first char of the pair
      else {
        // recurse to 2 spawned pairs
        val c = rules(pair)
        go(List(pair.head, c).mkString, depth - 1)
        go(List(c, pair.last).mkString, depth - 1)
      }
    }

    val pairs = template.sliding(2).toList
    pairs.foreach(p => go(p, depth))
    val counts = count(acc, template.last)
    //val counts0 = go(mutable.Map[Char, Long](), "NN", 2)
     //val counts = count(counts0, 'N')

    println(counts)
    println(counts.values.sum)
    counts.values.max - counts.values.min
  }

  def count(map: mutable.Map[Char, Long], c: Char): mutable.Map[Char, Long] = {
    //println(s"Counting: $c")
    map.get(c) match {
      case Some(n) => map.addOne((c, n + 1))
      case None    => map.addOne((c, 1))
    }
  }

  //TODO: pfp p22
  trait Counter[F[_]] {
    def incr: F[Unit]
    def get: F[Int]
  }


  run(fs2.io.file.Path("data/day14test.txt"))
  //run(fs2.io.file.Path("data/day14input.txt"))
}
