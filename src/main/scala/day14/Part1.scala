package day14

import scala.collection.mutable

object Part1 extends App with Utils {

  def solve(template: Polymer, rules: Map[String, Char]): Long = {
    val polymer = (0 until 10).foldLeft(template) { (polymer, n) =>
      //println(n)
      applyInsertions(polymer, rules)
    }

    val counts = count(polymer)

    val max = counts.values.max
    val min = counts.values.min

    max - min
  }

  def insert(pair: String, c: Char): String = pair.toList match {
    case a :: b :: Nil => List(a, c).mkString //don't duplicate b
  }

  def applyInsertions(polymer: String, rules: Map[String, Char]): String = {
    polymer.sliding(2).toList.map { p =>
      insert(p, rules(p))
    }.mkString :+ polymer.last
  }

  def count(polymer: Polymer): Map[Char, Long] = {
    polymer.toList.foldLeft(mutable.Map[Char, Long]()) { (map, c) =>
      map.get(c) match {
        case Some(n) => map.addOne((c, n + 1))
        case None    => map.addOne((c, 1))
      }
    }.toMap
  }

  run(fs2.io.file.Path("data/day14test.txt"))
  //run(fs2.io.file.Path("data/day14input.txt"))
}
