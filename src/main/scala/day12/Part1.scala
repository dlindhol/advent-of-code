package day12

object Part1 extends App with Utils {

  def solve(caves: CaveSystem): Int = {
    def go(path: Path): List[Path] = {
      if (path.last == end) List(path)
      else caves.neighbors(path.last).flatMap { c =>
        if (c.isInstanceOf[SmallCave] && path.contains(c)) List(path) //dead end
        else go(path :+c) //add cave to path and recurse
      }
    }
    val paths = go(List(start)).filter(_.last == end)
    paths
      .map(p => p.map(_.name).mkString(","))
      .foreach(println)
    paths.length
  }

  run(fs2.io.file.Path("data/day12test1.txt"))
  //run(fs2.io.file.Path("data/day12input.txt"))
}
