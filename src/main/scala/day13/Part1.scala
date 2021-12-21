package day13

object Part1 extends App with Utils {

  def solve(dots: List[Dot], folds: List[Fold]): NumberOfDots = {
    folds.take(1).foldLeft(dots) { (ds, f) => f match {
      case Fold('x', i) => foldDotsHorizontally(ds, i)
      case Fold('y', i) => foldDotsVertically(ds, i)
    }}.length
  }

  //run(fs2.io.file.Path("data/day13test.txt"))
  run(fs2.io.file.Path("data/day13input.txt"))
}
