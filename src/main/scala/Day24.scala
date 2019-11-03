package advent

object Day24 {

  def day24(): Unit = {
    val map = readFile(inputFile)
    println(s"Day24.part1 = ${part1(map)}")
    println(s"Day24.part2 = ${part2(map)}")
  }

  def part1(map: PuzzleMap): Int =
    findPath(map).length

  def part2(map: PuzzleMap): Int =
    findPathAndBack(map).length

  sealed trait Thing
  case object Wall        extends Thing
  case class Target(i: Int) extends Thing

  case class Coord(x: Int, y: Int) {
    def up    = Coord(x,   y-1)
    def down  = Coord(x,   y+1)
    def left  = Coord(x-1, y)
    def right = Coord(x+1, y)
  }

  type PuzzleMap = Map[Coord, Thing]
  type Path      = (List[Thing], Int)  // List of targets and total path length
  val EmptyPath  = (List(), Int.MaxValue)

  case class State(map: PuzzleMap, c: Coord, length: Int, history: List[Thing])

  def findPath(map: PuzzleMap): State = {
    def helper(solution: State, working: List[State]): State = {
      working.sortBy(_.length) match {
        case Nil    => solution
        case h :: t =>
          val next = pathsFrom(h).filter(_.length < solution.length)
          val (s, n) = next.partition(s => isEmpty(s.map))
          val nextSolution = s.sortBy(_.length).headOption.getOrElse(solution)
          helper(nextSolution, n ++ t)
      }
    }

    helper(State(map, Coord(0, 0), Int.MaxValue, List()), List(State(map, findStart(map), 0, List())))
  }

  def findPathAndBack(map: PuzzleMap): State = {
    val start = findStart(map)

    def helper(solution: State, working: List[State]): State = {
      working.sortBy(_.length) match {
        case Nil    => solution
        case h :: t =>
          val next = pathsFrom(h).filter(_.length < solution.length)
          val (s, n) = next.partition(s => isEmpty(s.map) && s.c == start)
          val nn =
            next.map {
              case s if isEmpty(s.map) => s.copy(map = s.map + (start -> Target(0)))
              case s                   => s
            }
          val nextSolution = s.sortBy(_.length).headOption.getOrElse(solution)
          helper(nextSolution, nn ++ t)
      }
    }

    helper(State(map, Coord(0, 0), Int.MaxValue, List()), List(State(map, start, 0, List())))
  }

  def pathsFrom(state: State): List[State] = {
    def helper(accum: List[State], visited: Set[Coord], next: Set[Coord], d: Int): List[State] = {
      if(targetsRemaining(state.map, visited) == 0 || next.isEmpty) accum
      else {
        val nextAccum = accum ++ next.filter(isTarget(state.map, _)).map(c => State(collectThing(state.map, c), c, state.length + d, state.map(c) +: state.history)).toList
        val nextCoords = next.map(availableSteps(state.map, _)).flatten -- visited
        helper(nextAccum, visited ++ next, nextCoords, d+1)
      }
    }

    helper(List.empty[State], Set.empty[Coord], Set(state.c), 0)
  }

 def findStart(map: PuzzleMap): Coord =
    map.collect { case (c, Target(0)) => c }.head

  def availableSteps(map: PuzzleMap, c: Coord): Set[Coord] =
    Set(c.up, c.down, c.left, c.right).filter(isOpen(map, _))

  def isOpen(map: PuzzleMap, c: Coord): Boolean =
    map.get(c) != Some(Wall)

  def isTarget(map: PuzzleMap, c: Coord): Boolean =
    map.get(c) match {
      case Some(Target(_)) => true
      case Some(Wall)      => false
      case None            => false
    }

  def isEmpty(map: PuzzleMap): Boolean =
    (map.collect { case (_, Target(t)) => t }).size == 0

  def targetsRemaining(map: PuzzleMap, visited: Set[Coord]): Int =
    (map.keySet -- visited).size

  def collectThing(map: PuzzleMap, c: Coord): PuzzleMap =
    map - c

  def parseThing(c: Char): Option[Thing] = {
    c match {
      case '#'            => Some(Wall)
      case d if d.isDigit => Some(Target(d - '0'))
      case '.'            => None
    }
  }

  def parseRow(r: Int, s: String): List[(Coord, Thing)] =
    LazyList.from(0).zip(s).foldLeft(List.empty[(Coord, Thing)]) { case (a, (i, c)) =>
      parseThing(c) match {
        case Some(t) => (Coord(r, i), t) +: a
        case None    => a
      }
    }

  def readFile(f: String): PuzzleMap =
    LazyList.from(0)
      .zip(io.Source.fromFile(f).getLines())
      .map { case (r, s) => parseRow(r, s)  }
      .flatten
      .toMap

  val inputFile = "data/Day24.txt"
  val testData  = "data/Day24-small.txt"
}
