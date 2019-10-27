package advent

object Day17 {

  def day17(): Unit = {
    println(s"Day17.part1 = ${part1(puzzleInput)}")
    println(s"Day17.part2 = ${part2(puzzleInput)}")
  }

  def part1(passcode: String): Option[String] =
    traverse(Set(State(start, "", passcode)), dest)

  def part2(passcode: String): Option[Int] =
    findLongest(State(start, "", passcode), dest)

  sealed trait Direction { def show: Char }

  case object Up    extends Direction { def show = 'U' }
  case object Down  extends Direction { def show = 'D' }
  case object Left  extends Direction { def show = 'L' }
  case object Right extends Direction { def show = 'R' }

  case class State(pos: Coord, path: String, passcode: String)

  def traverse(states: Set[State], dest: Coord): Option[String] = {
    if(states.isEmpty) None
    else
      states.find(_.pos == dest) match {
        case Some(s) => Some(s.path)
        case None    => traverse(states.map(nextStates).flatten, dest)
      }
  }

  def findLongest(state: State, dest: Coord): Option[Int] = {
    def helper(longest: Option[Int], states: Set[State]): Option[Int] =
      if(states.isEmpty) longest
      else {
        val (d, r) = states.partition(_.pos == dest)
        val long = d.toList.sortBy(_.path.length).reverse.headOption.map(_.path.size)
        val newLongest =
          (longest, long) match {
            case (None, Some(n))    => long
            case (Some(o), None)    => longest
            case (Some(o), Some(l)) => Some(Math.max(o, l))
            case (None, None)       => None
          }
        helper(newLongest, r.map(nextStates).flatten)
      }

    helper(None, Set(state))
  }

  def nextStates(state: State): Set[State] = {
    state match { case State(pos, path, passcode) =>
      val open = openDoors(md5HashString(passcode + path))
      val moves =
        open
          .map(d => (d, move(pos, d)))
          .collect { case (d, Some(c)) => (d, c) }

      moves.map { case (d, c) => State(c, path + d.show, passcode) }
    }
  }

  def move(p: Coord, dir: Direction): Option[Coord] =
    dir match {
      case Up    => if(p.r > 0) Some(p.copy(r = p.r-1)) else None
      case Down  => if(p.r < 3) Some(p.copy(r = p.r+1)) else None
      case Left  => if(p.c > 0) Some(p.copy(c = p.c-1)) else None
      case Right => if(p.c < 3) Some(p.copy(c = p.c+1)) else None
    }

  def isOpen(c: Char): Boolean =
    if(c >= 'b') true
    else        false

  def openDoors(code: String): Set[Direction] =
    List(Up, Down, Left, Right)
      .zip(code.take(4))
      .filter { case (d, c) =>  isOpen(c)}
      .map(_._1)
      .toSet

  def md5HashString(s: String) = {
    import java.security.MessageDigest

    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map(b => f"$b%02x")
      .mkString
  }

  case class Coord(r: Int, c: Int)
  val start = Coord(0, 0)
  val dest  = Coord(3, 3)

  val puzzleInput = "njfxhljp"

  val testInput1 = "ihgpwlah"
  val output1 = "DDRRRD"

  val testInput2 = "kglvqrro"
  val output2 = "DDUDRLRRUDRD"

  val testInput3 = "ulqzkmiv"
  val output3 = "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
}
