package day8

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream

class TestSuite extends munit.FunSuite {

  /** Streams test data. */
  def testData: Stream[IO, String] = Stream.emits(
    """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
    """.split("\n").toList
  )

  val testLine = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

  test("pattern") {
    val s = pattern.findFirstIn(testLine)
    assertEquals(s, Some(testLine))
  }

  test("parse") {
    testData.through(clean).through(parse).compile.toList.map { entries =>
      entries.head match {
        case Entry(digits, display) =>
          assertEquals(digits.length, 10)
          assertEquals(display.length, 4)
      }
    }.unsafeRunSync()
  }

  test("solve part 1") {
    Part1.solve(testData).map { r =>
      assertEquals(r, 26)
    }.unsafeRunSync()
  }

  val Zero  = parseDigit("abcefg")
  val One   = parseDigit("cf")
  val Two   = parseDigit("acdeg")
  val Three = parseDigit("acdfg")
  val Four  = parseDigit("bcdf")
  val Five  = parseDigit("abdfg")
  val Six   = parseDigit("abdefg")
  val Seven = parseDigit("acf")
  val Eight = parseDigit("abcdefg")
  val Nine  = parseDigit("abcdfg")

  test("id digits") {
    val ds = Part2.idDigits(List(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine))
    //ds.foreach(println)
    assertEquals(ds(0), Zero)
    assertEquals(ds(1), One)
    assertEquals(ds(2), Two)
    assertEquals(ds(3), Three)
    assertEquals(ds(4), Four)
    assertEquals(ds(5), Five)
    assertEquals(ds(6), Six)
    assertEquals(ds(7), Seven)
    assertEquals(ds(8), Eight)
    assertEquals(ds(9), Nine)
  }

  test("read display") {
    val e = parseEntry(testLine)
    //val e = parseEntry("abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg | cf acdeg acdfg bcdf")
    val n = Part2.readDisplay(e)
    assertEquals(n, 5353)
  }

  test("solve part 2") {
    Part2.solve(testData).map { r =>
      assertEquals(r, 61229)
    }.unsafeRunSync()
  }
}
