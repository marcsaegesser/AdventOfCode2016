package advent

object Day08 {

  def day08(): Unit = {
    val instructions = readFile(inputFile)
    println(s"Day08.part1 = ${part1(instructions)}")
    println(s"Day08.part2 = \n${part2(instructions)}")
  }

  def part1(instructions: List[Instruction]): Int =
    countPixels(runInstructions(instructions, initDisplay(displayWidth, displayHeight)))

  def part2(instructions: List[Instruction]): String =
    showDisplay(runInstructions(instructions, initDisplay(displayWidth, displayHeight)))

  type Display = Vector[Vector[Char]]
  val off = '.'
  val on  = '#'

  val displayWidth  = 50
  val displayHeight = 6

  def countPixels(display: Display): Int =
    display.flatten.filter(_ == on).size

  def runInstructions(is: List[Instruction], display: Display): Display =
    is.foldLeft(display) { case (d, i) =>
      runInstruction(i, d)
    }

  def runInstruction(i: Instruction, display: Display): Display =
    i match {
      case Rect(a, b)      => drawRect(a, b, display)
      case RotateRow(y, n) => rotRow(y, n, display)
      case RotateCol(x, n) => rotCol(x, n, display)
    }

  def drawRect(a: Int, b: Int, display: Display): Display = {
    (0 until b).foldLeft(display) { case (d, r) =>
      val row = getRow(r, d)
      val newRow = Vector.fill(a)(on) ++ row.drop(a)
      setRow(r, newRow, d)
    }
  }

  def rotRow(y: Int, n: Int, display: Display): Display =
    setRow(y, rotateVector(n, getRow(y, display)), display)

  def rotCol(x: Int, n: Int, display: Display): Display =
    setCol(x, rotateVector(n, getCol(x, display)), display)

  def rotateVector(n: Int, v: Vector[Char]): Vector[Char] = {
    val (h, t) = v.splitAt(v.size - n)
    t ++ h
  }


  def getRow(y: Int, display: Display): Vector[Char] =
    display(y)

  def setRow(y: Int, row: Vector[Char], display: Display): Display =
    display.updated(y, row)

  def getCol(x: Int, display: Display): Vector[Char] =
    display.transpose.apply(x)

  def setCol(x: Int, col: Vector[Char], display: Display): Display =
    display.transpose.updated(x, col).transpose

  def getHeight(display: Display): Int =
    display.size

  def getWidth(display: Display): Int =
    display(0).size

  def initDisplay(a: Int, b: Int): Display =
    Vector.fill(b)(Vector.fill(a)(off))

  def showDisplay(display: Display): String =
    display.map(_.mkString).mkString("\n")


  sealed trait Instruction
  case class Rect(a: Int, b: Int)   extends Instruction
  case class RotateRow(y: Int, n: Int) extends Instruction
  case class RotateCol(x: Int, n: Int) extends Instruction

  val rectRegex   = """rect\s+(\d+)x(\d+)""".r
  val rotRowRegex = """rotate row y=(\d+)\s+by\s+(\d+)""".r
  val rotColRegex = """rotate column x=(\d+)\s+by\s+(\d+)""".r

  def parseInstruction(s: String): Instruction =
    s match {
      case rectRegex(a, b)   => Rect(a.toInt, b.toInt)
      case rotRowRegex(y, n) => RotateRow(y.toInt, n.toInt)
      case rotColRegex(x, n) => RotateCol(x.toInt, n.toInt)
    }

  def readFile(f: String): List[Instruction] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseInstruction)
      .toList

  val inputFile = "data/Day08.txt"

  val testData = List("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1")
}
