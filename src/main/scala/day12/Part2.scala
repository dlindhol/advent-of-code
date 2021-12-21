package day12

object Part2 extends App with Utils {

  def solve(caves: CaveSystem): Int = {
    def go(path: Path): List[Path] = {
      if (path.last == end) List(path)
      else caves.neighbors(path.last).toList.flatMap { c =>
        if (c.isInstanceOf[SmallCave] && path.contains(c)) List(path :+ c) //dead end
        else go(path :+c) //add cave to path and recurse
      }
    }

    // First pass with no small cave revisits
    val paths = go(Path(start))

    // Second pass to allow revisit of one small cave
    val extraPaths = paths
      .filter(endsWithReplicableSmallCave)
      .flatMap(go)

    (paths ++ extraPaths).count(_.last == end)
  }

  def endsWithReplicableSmallCave(path: Path): Boolean = {
    val last = path.last
    last.isInstanceOf[SmallCave] &&
      last != start &&
      last != end
  }


  //run(fs2.io.file.Path("data/day12test3.txt"))
  run(fs2.io.file.Path("data/day12input.txt"))
}
