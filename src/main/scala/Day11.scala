package advent

/* Really not happey with this solution, but it does give the right
 * answer.
 */
object Day11 {

  sealed trait Item { def element: String }
  case class M(element: String) extends Item  // Microchip
  case class G(element: String) extends Item  // Generator

  case class State(floors: Map[Int, Set[Item]], elevatorFloor: Int) {
    override def equals(other: Any): Boolean =
      other match {
        case s: State => areEquiv(this, s)
        case _        => false
      }
  }

  def sig(state: State): String = {
    def floorSig(f: Set[Item]): String =
      f.toList.map {
        case M(_) => "M"
        case G(_) => "G"
      }.sorted.mkString

    state.elevatorFloor.toString ++ (1 to 4).map(i => floorSig(state.floors(i))).mkString("|")
  }

  def areEquiv(a: State, b: State): Boolean = {
    if(sig(a) != sig(b)) false
    else {
      (1 to 4)
        .map(i => (a.floors(i) -- b.floors(i)) ++ (b.floors(i) -- a.floors(i)))
        .map(_.size%2 == 0)
        .reduce(_ && _)
    }
  }

  def solve(initial: State): Option[Int] = {
    def helper(visited: Set[State], active: Set[State], steps: Int): Option[Int] = {
      println(s"visited.size=${visited.size}, active.size=${active.size}")
      if(active.isEmpty) None
      else {
        val next = active.flatMap(statesFrom) -- visited
        val (s, r) = next.partition(isSolved)
        if(!s.isEmpty) Some(steps)
        else           helper(visited ++ r, r, steps+1)
      }
    }

    helper(Set(initial), Set(initial), 1)
  }

 def statesFrom(state: State): List[State] = {
    def unloadElevator(from: Int, to: Int, loads: List[(Set[Item], Set[Item])]): List[State] = {
      state.floors.get(to)
        .map(f =>
          loads
            .map { case (e, r) => (e ++ f, r) }
            .filter(ef => isSafe(ef._1))
            .map { case (e, r) => state.floors.updated(from, r).updated(to, e) }
            .map(fs => State(fs, to))
        ).getOrElse(List.empty[State])
    }

    val fs = state.floors(state.elevatorFloor)
    val loads =   // Safe elevator/remaining loads
      (fs.subsets(1) ++ fs.subsets(2))
        .map(s => (s, fs -- s))
        .filter { case (e, f) => isSafe(e) && isSafe(f) }.toList

    val up = unloadElevator(state.elevatorFloor, state.elevatorFloor+1, loads)
    val down = unloadElevator(state.elevatorFloor, state.elevatorFloor-1, loads)

    up ++ down
  }

  def isSolved(state: State): Boolean =
    (1 to 3).map(i => state.floors(i).size).sum == 0

  def isSafe(items: Set[Item]): Boolean = {
    val (ms, gs) =  // Microships and Generators
      items.foldLeft((Set.empty[String], Set.empty[String])) {
        case ((ms, gs), M(e)) => (ms + e, gs)
        case ((ms, gs), G(e)) => (ms,     gs+e)
      }

    (!(ms -- gs).isEmpty, !gs.isEmpty) match {
      case (true, true) => false  // Unmatched chips with any generators
      case (true, false) => true  // Unmatched chips but no generators
      case (false, _)  => true  // No unmatched chips is always safe
    }
  }

  val floorRegex = """The (\w+) floor.*""".r
  val chipRegex  = """(\w+)-compatible microchip""".r
  val genRegex   = """(\w+) generator""".r

  def parseFloorNumber(s: String): Int =
    s match {
      case floorRegex(f) if f == "first"  => 1
      case floorRegex(f) if f == "second" => 2
      case floorRegex(f) if f == "third"  => 3
      case floorRegex(f) if f == "fourth" => 4
    }

  def parseItems(s: String): Set[Item] = {
    chipRegex.findAllMatchIn(s).map(m => M(m.group(1))).toSet ++
    genRegex.findAllMatchIn(s).map(m => G(m.group(1))).toSet
  }

  def parseFloor(s: String): (Int, Set[Item]) = {
    val floor = parseFloorNumber(s)
    val items = parseItems(s)
    (floor, items)
  }

  def mkState(floors: Map[Int, Set[Item]]) =
    State(floors, elevatorFloor = 1)

  def readFile(f: String): State = {
    val floors =
      io.Source.fromFile(f).getLines()
        .map(parseFloor)
        .toMap

    mkState(floors)
  }

  val inputFile = "data/Day11.txt"
  val input2File = "data/Day11-2.txt"
  val testFile = "data/Day11-small.txt"
}
