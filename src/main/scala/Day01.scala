package advent

object Day01 {

  def day01(): Unit = {
    val instructions = readFile(inputFile)
    println(s"Day01.part1 = ${part1(instructions)}")
    println(s"Day01.part2 = ${part2(instructions)}")
  }

  def part1(instructions: List[Instruction]): Int = {
    val (x, y) = followInstructions(initialState, instructions)
    Math.abs(x) + Math.abs(y)
  }

  def part2(instructions: List[Instruction]): Int = {
    val (x, y) = followInstructions2(initialState, instructions)
    Math.abs(x) + Math.abs(y)
  }

  sealed trait Heading
  case object North extends Heading
  case object East  extends Heading
  case object South extends Heading
  case object West  extends Heading

  sealed trait Direction
  case object Right extends Direction
  case object Left  extends Direction

  case class State(heading: Heading, x: Int, y: Int, history: Set[(Int, Int)])

  val initialState = State(North, 0, 0, Set((0, 0)))

  case class Instruction(direction: Direction, steps: Int)

  def followInstructions(state: State, instructions: List[Instruction]): (Int, Int) =
    instructions match {
      case Nil => (state.x, state.y)
      case Instruction(d, s) :: t =>
        followInstructions(
          headingFrom(state.heading, d) match {
            case North => state.copy(heading = North, y = state.y + s)
            case East  => state.copy(heading = East,  x = state.x + s)
            case South => state.copy(heading = South, y = state.y - s)
            case West  => state.copy(heading = West,  x = state.x - s)
          }, t)
    }

  def followInstructions2(state: State, instructions: List[Instruction]): (Int, Int) =
    instructions match {
      case Nil => throw new Exception("Uh oh.")
      case Instruction(d, s) :: t =>
        val (h, x, y, coords) =
          headingFrom(state.heading, d) match {
            case North => (North, state.x,     state.y + s, (1 to s).map(y => (state.x, state.y+y)))
            case East  => (East,  state.x + s, state.y,     (1 to s).map(x => (state.x+x, state.y)))
            case South => (South, state.x,     state.y - s, (1 to s).map(y => (state.x, state.y-y)))
            case West  => (West,  state.x - s, state.y,     (1 to s).map(x => (state.x-x, state.y)))
          }
        coords.find(c => state.history.contains(c)) match {
          case Some(c) => c
          case None    => followInstructions2(State(h, x, y, state.history ++ coords), t)
        }
    }


  def headingFrom(heading: Heading, direction: Direction): Heading =
    heading match {
      case North if direction == Right => East
      case North if direction == Left  => West
      case East  if direction == Right => South
      case East  if direction == Left  => North
      case South if direction == Right => West
      case South if direction == Left  => East
      case West  if direction == Right => North
      case West  if direction == Left  => South
    }

  val instructionRegex = """([RL])(\d+)""".r

  def parseInstruction(s: String): Instruction =
    s.trim match {
      case instructionRegex("R", s) => Instruction(Right, s.toInt)
      case instructionRegex("L", s) => Instruction(Left, s.toInt)
    }

  def readFile(f: String): List[Instruction] =
    io.Source.fromFile(f)
      .getLines()
      .mkString
      .split(",")
      .toList
      .map(parseInstruction)

  val inputFile = "data/Day01.txt"
}
