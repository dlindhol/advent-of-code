package day14

import scala.collection.mutable

object Part2b extends App with Utils {

  val depth = 40

  /**
   * Counts pairs at each step so insert only needs to happen for each
   * unique pair with a multiplier. Then we count the characters.
   */
  def solve(template: Polymer, rules: Map[String, Char]): Long = {
    // Count the pairs from the next round of insertions
    def go(counter: Counter[String]): Counter[String] = {
      val newCounter = Counter.make[String]
      counter.items.foreach { p =>
        val n = counter.getCount(p)
        val c = rules(p)
        newCounter.count(List(p.head, c).mkString, n)
        newCounter.count(List(c, p.last).mkString, n)
      }
      newCounter
    }

    // Count pairs through "depth" recursions
    val pairCounts = {
      val counter = Counter.make[String]
      template.sliding(2).toList.foreach(counter.count(_))
      (0 until depth).foldLeft(counter) { (ctr, _) =>
        //println(ctr)
        go(ctr)
      }
    }

    // Count individual characters
    val counts = {
      val counter = Counter.make[Char]
      pairCounts.items.foreach { p =>
        //println(p, pairCounts.getCount(p))
        val n = pairCounts.getCount(p)
        counter.count(p.head, n)
      }
      counter.count(template.last) //count last char
      counter
    }

    //println(counts)
    (for {
      max <- counts.getMaxCount
      min <- counts.getMinCount
    } yield max._2 - min._2).get
  }

  run(fs2.io.file.Path("data/day14test.txt"))
  //run(fs2.io.file.Path("data/day14input.txt"))
}

trait Counter[A] {
  def count(a: A, mult: Long = 1L): Unit
  def items: List[A]
  def getCount(a: A): Long
  def getMaxCount: Option[(A, Long)]
  def getMinCount: Option[(A, Long)]
}

object Counter {
  def make[A]: Counter[A] = new Counter[A] {
    private val map = mutable.Map[A, Long]()

    def count(a: A, mult: Long = 1L): Unit = map.get(a) match {
      case Some(n) => map.addOne((a, n + mult))
      case None    => map.addOne((a, mult))
    }
    def items: List[A] = map.keys.toList
    def getCount(a: A): Long = map.getOrElse(a, 0L)
    def getMaxCount: Option[(A, Long)] = map.maxByOption(_._2)
    def getMinCount: Option[(A, Long)] = map.minByOption(_._2)

    override def toString: String = map.toString()
  }
}
