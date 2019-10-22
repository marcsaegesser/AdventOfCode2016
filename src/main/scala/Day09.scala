package advent

object Day09 {

  def day09(): Unit = {
    val input = readFile(inputFile)
    println(s"Day09.part1 = ${part1(input)}")
    println(s"Day09.part2 = ${part2(input)}")
  }

  def part1(input: String): Int =
    decompress(input).size

  def part2(input: String): Long =
    decompress2(input)

  val repeatRegex = """\((\d+)x(\d+)\)(.*)""".r
  val constantRegex = """([A-Z]+)(.*)""".r

  def decompress(input: String): String = {
    def helper(accum: String, remaining: String): String =
      remaining match {
        case ""                   => accum
        case constantRegex(h, t)  => helper(accum ++ h, t)
        case repeatRegex(n, r, t) =>
          val (pattern, rest) = t.splitAt(n.toInt)
          helper(accum ++ List.fill(r.toInt)(pattern).mkString, rest)
      }

    helper("", input)
  }

  def decompress2(s: String): Long = {
    def helper(remaining: String): Long = {
      remaining match {
        case "" => 0L
        case constantRegex(h, t) => h.size + helper(t)
        case repeatRegex(n, r, t) =>
          val (segment, rest) = t.splitAt(n.toInt)
          r.toInt*helper(segment) + helper(rest)
      }
    }

    helper(s)
  }

  def readFile(f: String): String =
    io.Source.fromFile(f)
      .getLines
      .toList
      .flatten
      .mkString

  val inputFile = "data/Day09.txt"
}
