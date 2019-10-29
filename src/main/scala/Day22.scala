package advent

object Day22 {
  def day22(): Unit = {
    val cluster = readFile(puzzleData)
    println(s"Day22.part1 = ${part1(cluster)}")
    println(s"Day22.part2 = ${part2()}")
  }

  def part1(cluster: Cluster): Int =
    countViable(cluster)

  // By looking at the map there is only one node that accept any
  // data. It takes 35 steps to move the data in the next node
  // out of the way and move the goal node one node left. After that
  // The only empty node is the one just vacated by the goal. From
  // that point it takes 5 steps to move the node around the goal
  // node the just vacated node and move the goal one more step left.
  // 36 such steps are necessary.
  def part2(): Int = {
    35 + 5*36
  }

  def findGoal(cluster: Cluster): Coord =
    cluster.keys.filter(_.y == 0).maxBy(_.x)

  def countViable(cluster: Cluster): Int = {
    cluster.keys.toList
      .combinations(2)
      .foldLeft(0) { case (a, List(locA, locB)) =>
        a +
        (if(isViable(locA, locB, cluster)) 1 else 0) +
        (if(isViable(locB, locA, cluster)) 1 else 0)
      }
  }

  def findViable(cluster: Cluster): List[(Coord, Coord)] =
    cluster.keys
      .toList
      .combinations(2)
      .collect {
        case List(locA, locB) if(isViable(locA, locB, cluster)) => (locA, locB)
        case List(locA, locB) if(isViable(locB, locA, cluster)) => (locB, locB)
      }.toList

  def isViable(locA: Coord, locB: Coord, cluster: Cluster): Boolean = {
    val a = cluster(locA)
    val b = cluster(locB)
    a.used > 0 && b.avail >= a.used
  }


  case class Coord(x: Int, y: Int) {
    def up    = Coord(x,   y-1)
    def down  = Coord(x,   y+1)
    def right = Coord(x+1, y)
    def left  = Coord(x-1, y)
  }
  case class Node(loc: Coord, size: Int, used: Int, avail: Int, use: Int)
  type Cluster = Map[Coord, Node]

  def selectRow(cluster: Cluster, y: Int): List[Node] =
    cluster.collect { case (Coord(_, cy), n) if cy == y => n }.toList.sortBy(_.loc.x)
    // cluster.filter { case (Coord(_, cy), _) => cy == y }.toList.sortBy { case (c, n) => c.x }

  def showRow(cluster: Cluster, y: Int): String =
    selectRow(cluster, y).map(n => s"${n.used}/${n.size}").mkString(" ")

  def showCluster(cluster: Cluster): String = {
    val (_, maxY) = bounds(cluster)
    (0 to maxY).map(y => showRow(cluster, y)).mkString("\n")
  }

  def bounds(cluster: Cluster): (Int, Int) = {
    (cluster.keys.maxBy(_.x).x, cluster.keys.maxBy(_.y).y)
  }

  val nodeRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  def parseNode(s: String): Node =
    s match {
      case nodeRegex(x, y, s, u, a, p) => Node(Coord(x.toInt, y.toInt), s.toInt, u.toInt, a.toInt, p.toInt)
    }

  def readFile(f: String): Cluster =
    io.Source.fromFile(f)
      .getLines().drop(2)
      .map(parseNode)
      .map(n => (n.loc, n))
      .toMap


  val puzzleData = "data/Day22.txt"
}
