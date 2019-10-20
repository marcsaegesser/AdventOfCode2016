package advent

object Day02 {

  def day02(): Unit = {
    val moves = readFile(inputFile)
    println(s"Day02.part1 = ${part1(moves)}")
    println(s"Day02.part2 = ${part2(moves)}")
  }

  def part1(moves: Vector[Moves]): String = {
    val (_, coords) =
      moves.foldLeft((Coord(1, 1), List.empty[Coord])) { case ((c, l), ms) =>
        val next = runMoves(c, keyPad1, ms)
        (next, next +: l)
      }

    coords.reverse.map(keyPad1).mkString
  }

  def part2(moves: Vector[Moves]): String = {
    val (_, coords) =
      moves.foldLeft((Coord(1, 1), List.empty[Coord])) { case ((c, l), ms) =>
        val next = runMoves(c, keyPad2, ms)
        (next, next +: l)
      }

    coords.reverse.map(keyPad2).mkString
  }

  sealed trait Move
  case object Up    extends Move
  case object Right extends Move
  case object Left  extends Move
  case object Down  extends Move

  type Moves = List[Move]

  val minRow = 0
  val maxRow = 2
  val minCol = 0
  val maxCol = 2

  case class Coord(r: Int, c: Int)

  def runMoves(coord: Coord, keypad: Map[Coord, Char], moves: Moves): Coord =
    moves match {
      case Nil => coord
      case h :: t =>
        val next =
          h match {
            case Up    => coord.copy(r = coord.r - 1)
            case Right => coord.copy(c = coord.c + 1)
            case Left  => coord.copy(c = coord.c - 1)
            case Down  => coord.copy(r = coord.r + 1)
          }
        if(keypad.isDefinedAt(next)) runMoves(next, keypad, t)
        else                         runMoves(coord, keypad, t)
    }

  val keyPad1: Map[Coord, Char] =
    Map(
      Coord(0, 0) -> '1', Coord(0, 1) -> '2', Coord(0, 2) -> '3',
      Coord(1, 0) -> '4', Coord(1, 1) -> '5', Coord(1, 2) -> '6',
      Coord(2, 0) -> '7', Coord(2, 1) -> '8', Coord(2, 2) -> '9'
    )

  val keyPad2: Map[Coord, Char] =
    Map(
      Coord(0, 2) -> '1',
      Coord(1, 1) -> '2', Coord(1, 2) -> '3', Coord(1, 3) -> '4',
      Coord(2, 0) -> '5', Coord(2, 1) -> '6', Coord(2, 2) -> '7', Coord(2, 3) -> '8', Coord(2, 4) -> '9',
      Coord(3, 1) -> 'A', Coord(3, 2) -> 'B', Coord(3, 3) -> 'C',
      Coord(4, 2) -> 'D'
    )

  def parseLine(s: String): Moves =
    s.toSeq.map {
      _ match {
        case 'U' => Up
        case 'R' => Right
        case 'L' => Left
        case 'D' => Down
      }
    }.toList

  def readFile(f: String): Vector[Moves] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseLine)
      .toVector

  val inputFile = "data/Day02.txt"
}
