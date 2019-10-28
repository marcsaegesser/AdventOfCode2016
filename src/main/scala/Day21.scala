package advent

object Day21 {

  def part1(input: String, ops: List[Operation]): String =
    applyOperations(input, ops)


  def applyOperations(string: String, operations: List[Operation]): String =
    operations.foldLeft(string) { case (s, o) =>
      applyOperation(s, o)
    }

  def applyOperation(string: String, operation: Operation): String =
    operation match {
      case SwapPositions(p1, p2)    => swapPositions(string, p1, p2)
      case SwapLetters(l1, l2)      => swapLetters(string, l1, l2)
      case RotateLeft(n)            => rotateLeft(string, n)
      case RotateRight(n)           => rotateRight(string, n)
      case RotateOnPosition(l)      => rotateOnPosition(string, l)
      case ReversePositions(p1, p2) => reversePositions(string, p1, p2)
      case MovePosition(p1, p2)     => movePosition(string, p1, p2)
    }

  def unapplyOperation(string: String, operation: Operation): String =
    operation match {
      case SwapPositions(p1, p2)    => swapPositions(string, p1, p2)
      case SwapLetters(l1, l2)      => swapLetters(string, l1, l2)
      case RotateLeft(n)            => rotateRight(string, n)
      case RotateRight(n)           => rotateLeft(string, n)
      case RotateOnPosition(l)      => unrotateOnPosition(string, l)
      case ReversePositions(p1, p2) => reversePositions(string, p1, p2)
      case MovePosition(p1, p2)     => movePosition(string, p2, p1)
    }

  def swapPositions(string: String, p1: Int, p2: Int): String =
    string.updated(p1, string(p2)).updated(p2, string(p1))

  def swapLetters(string: String, l1: Char, l2: Char): String =
    swapPositions(string, string.indexOf(l1.toString), string.indexOf(l2.toString))

  def rotateLeft(string: String, n: Int): String = {
    val (a, b) = string.splitAt(n%string.size)
    b ++ a
  }

  def rotateRight(string: String, n: Int): String = {
    val (a, b) = string.splitAt(string.size - n%string.size)
    b ++ a
  }

  def rotateOnPosition(string: String, l: Char): String = {
    val idx = string.indexOf(l.toString)
    val n =
      if(idx >= 4) 1 + idx + 1
      else        1 + idx
    rotateRight(string, n)
  }

  val unRotateAmount =
    Map(
      1 -> 1,
      3 -> 2,
      5 -> 3,
      7 -> 4,
      2 -> 6,
      4 -> 7,
      6 -> 8,
      0 -> 9
    )

  def unrotateOnPosition(string: String, l: Char): String = {
    val idx = string.indexOf(l.toString)
    val n = unRotateAmount(idx)
    rotateLeft(string, n)
  }

  def reversePositions(string: String, p1: Int, p2: Int): String = {
    val (a, b) = string.splitAt(p1)
    val (c, d) = b.splitAt(p2-p1+1)
    a ++ c.reverse ++ d
  }

  def movePosition(string: String, p1: Int, p2: Int): String = {
    val c = string(p1)
    val (h, t) = string.splitAt(p1)
    val (a, b) = (h ++ t.tail).splitAt(p2)
    a + c ++ b
  }

  sealed trait Operation
  case class SwapPositions(p1: Int, p2: Int)     extends Operation
  case class SwapLetters(l1: Char, l2: Char) extends Operation
  case class ReversePositions(p1: Int, p2: Int)  extends Operation
  case class RotateLeft(n: Int)                extends Operation
  case class RotateRight(n: Int)               extends Operation
  case class RotateOnPosition(l: Char)       extends Operation
  case class MovePosition(p1: Int, p2: Int)      extends Operation

  val swapPositionRegex     = """swap position (\d+) with position (\d+)""".r
  val swapLetterRegex       = """swap letter ([a-z]) with letter ([a-z])""".r
  val rotateLeftRegex       = """rotate left (\d+) steps?""".r
  val rotateRightRegex      = """rotate right (\d+) steps?""".r
  val rotateOnPositionRegex = """rotate based on position of letter ([a-z])""".r
  val reversePositionsRegex = """reverse positions (\d+) through (\d+)""".r
  val movePositionRegex     = """move position (\d+) to position (\d+)""".r

  def parseOperation(s: String): Operation =
    s match {
      case swapPositionRegex(p1, p2)     => SwapPositions(p1.toInt, p2.toInt)
      case swapLetterRegex(l1, l2)       => SwapLetters(l1.head, l2.head)
      case rotateLeftRegex(n)            => RotateLeft(n.toInt)
      case rotateRightRegex(n)           => RotateRight(n.toInt)
      case rotateOnPositionRegex(l)      => RotateOnPosition(l.head)
      case reversePositionsRegex(p1, p2) => ReversePositions(p1.toInt, p2.toInt)
      case movePositionRegex(p1, p2)     => MovePosition(p1.toInt, p2.toInt)
    }

  def readFile(f: String): List[Operation] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseOperation)
      .toList

  val puzzleInput = "data/Day21.txt"
  val testInput = "data/Day21-small.txt"
}
