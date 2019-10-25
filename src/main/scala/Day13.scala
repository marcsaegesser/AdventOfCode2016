package advent

object Day13 {
  def day13(): Unit = {
    println(s"Day13.part1 = ${part1(puzzleInput)}")
    println(s"Day13.part2 = ${part2(puzzleInput)}")
  }

  def part1(secret: Int): Int =
    findPathLength(Coord(1, 1), Coord(31, 39), secret)._2

  def part2(secret: Int): Int =
    untilLength(Coord(1, 1), 50, secret)
      .filter { case (k, v) => v >= 0 && v <= 50 }
      .size

  case class Coord(x: Int, y: Int) {
    def up: Coord    = Coord(x, y-1)
    def down: Coord  = Coord(x, y+1)
    def left: Coord  = Coord(x-1, y)
    def right: Coord = Coord(x+1, y)
  }

  def adjacencies(c: Coord): Set[Coord] =
    Set(c.up, c.down, c.left, c.right)

  val Wall = -1
  type OfficeMap = Map[Coord, Int]  // True indicates coord is open space
  def emptyMap = Map.empty[Coord, Int]

  def findPathLength(start: Coord, target: Coord, secret: Int): (OfficeMap, Int) = {
    def helper(map: OfficeMap, dist: Int, next: Set[Coord]): (OfficeMap, Int) =
      if(next.isEmpty) (map, -1)  // No path found
      else if(next.contains(target)) {
        (map + (target -> dist), dist)
      } else {
        // Update board with new distances
        val b = map ++ next.map(c => (c, dist))
        // Compute next shell, updating board with discovered walls
        val (m, s) =
          next
            .map(adjacencies).flatten
            .foldLeft((b, Set.empty[Coord])) { case ((m, n), c) =>
            val (nm, open) = isOpen(m, c, secret)
            (nm, if(open) n + c else n)
          }
        // recurse
        helper(m, dist+1, s)
      }

    helper(emptyMap, 0, Set(start))
  }

  def untilLength(start: Coord, length: Int, secret: Int): OfficeMap = {
    def helper(map: OfficeMap, dist: Int, next: Set[Coord]): OfficeMap =
      if(dist > length) map
      else {
        // Update board with new distances
        val b = map ++ next.map(c => (c, dist))
        // Compute next shell, updating board with discovered walls
        val (m, s) =
          next
            .map(adjacencies).flatten
            .foldLeft((b, Set.empty[Coord])) { case ((m, n), c) =>
            val (nm, open) = isOpen(m, c, secret)
            (nm, if(open) n + c else n)
          }
        // recurse
        helper(m, dist+1, s)
      }

    helper(emptyMap, 0, Set(start))
  }

  def computeSilly(c: Coord, secret: Int): Boolean = {
    ((c.x*c.x + 3*c.x + 2*c.x*c.y + c.y + c.y*c.y + secret)
      .toBinaryString
      .filter(_ == '1')
      .size) % 2 == 0
  }

  def isOpen(map: OfficeMap, c: Coord, secret: Int): (OfficeMap, Boolean) =
    map.get(c) match {
      case Some(n) if n >= 0 => (map, false)  // Already visited location
      case Some(n) if n < 0 => (map, false)  // Wall
      case None =>
        val isOpen = c.x >= 0 && c.y >= 0 && computeSilly(c, secret)
        if(isOpen) (map, true)
        else       (map + (c -> Wall), false)
    }

  val puzzleInput = 1358
}
