package advent

object Day18 {

  def day18(): Unit = {
    val start = readFile(puzzleInput)
    println(s"Day18.part1 = ${part1(start, 40)}")
    println(s"Day18.part2 = ${part2(start, 400000)}")
  }

  def part1(input: String, length: Int): Int =
    computeSafe(input, length)

  def part2(input: String, length: Int): Int =
    computeSafe(input, length)

  val Wall = '#'
  val Safe = '.'
  val Trap = '^'

  def computeSafe(start: String, length: Int): Int = {
    def helper(safeCount: Int, row: String, rowCount: Int): Int =
      if(rowCount == length) safeCount + countSafe(row)
      else                  helper(safeCount + countSafe(row), nextRow(row), rowCount+1)

    helper(0, start, 1)
  }

  def countSafe(row: String): Int =
    row.filter(_ == Safe).size

  def countSafeMap(map: List[String]): Int =
    map.flatten.filter(_ == Safe).size

  def computeMap(start: String, length: Int): List[String] = {
    def helper(accum: List[String]): List[String] =
      if(accum.size == length) accum
      else
        accum match {
          case Nil => ???
          case h :: t => helper(nextRow(h) +: accum)
        }

    helper(List(start))
  }

  def nextRow(input: String): String =
    input.toSeq.sliding(3, 1).foldLeft(Wall.toString) { case (a, s) =>
      a + computeTile(s.mkString)
    } + Wall

  def computeTile(input: String): Char = {
    input.toSeq.map(c => isSafe(c)) match {
      case Vector(false, false, true) => Trap
      case Vector(true, false, false) => Trap
      case Vector(false, true, true) => Trap
      case Vector(true, true, false) => Trap
      case _               => Safe
    }
  }

  def isSafe(c: Char): Boolean =
    c == Wall || c == Safe

  def isTrap(c: Char): Boolean =
    c == Trap

  def readFile(f: String): String =
    io.Source.fromFile(f)
      .getLines().map(s => s"$Wall$s$Wall")
      .flatten
      .mkString

  val puzzleInput = "data/Day18.txt"

  val testInput1 = "#..^^.#"
  val testInput2 = "#.^^.^.^^^^#"
}
