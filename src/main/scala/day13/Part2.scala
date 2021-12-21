package day13

object Part2 extends App with Utils {

  def solve(dots: List[Dot], folds: List[Fold]): NumberOfDots = {
    val ds = folds.foldLeft(dots) { (ds, f) =>
      f match {
        case Fold('x', i) => foldDotsHorizontally(ds, i)
        case Fold('y', i) => foldDotsVertically(ds, i)
      }
    }
    printDots(ds)
    ds.length
  }

  def printDots(dots: List[Dot]):Unit = {
    val nx = dots.map(_._1).max
    val ny = dots.map(_._2).max
    (0 to ny).foreach { y =>
      (0 to nx).foreach { x =>
        if (dots.contains((x, y))) print("# ")
        else print("  ")
      }
      println()
    }
  }

  //run(fs2.io.file.Path("data/day13test.txt"))
  run(fs2.io.file.Path("data/day13input.txt"))
}
