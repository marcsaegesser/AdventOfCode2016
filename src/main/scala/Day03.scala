package advent

object Day03 {

  def part1(triples: Seq[Triple]): Int =
    triples.filter(isTriangle).size

  def part2(triples: Seq[Triple]): Int =
    triples.transpose
      .flatten
      .sliding(3, 3)
      .filter(x => isTriangle(x.toVector))
      .size

  def isTriangle(t: Triple): Boolean =
    rotations(t).forall { case Vector(a, b, c) => a + b > c }

  type Triple = Vector[Int]

  def rotations(triple: Triple): Seq[Triple] =
    Seq(
      triple,
      triple.tail :+ triple.head,
      triple.last +: triple.take(2)
    )

  val tripleRegex = """\s*(\d+)\s+(\d+)\s+(\d+)""".r

  def parseTriple(s: String): Triple =
    s match {
      case tripleRegex(a, b, c) => Vector(a.toInt, b.toInt, c.toInt)
    }

  def readFile(f: String): Seq[Triple] =
    io.Source.fromFile(f).getLines()
      .map(parseTriple)
      .toSeq


  val inputFile = "data/Day03.txt"
}
