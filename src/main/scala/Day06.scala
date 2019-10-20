package advent

object Day06 {

  def part1(msgs: List[String]): String =
    msgs
      .transpose
      .map(mostOccurrences)
      .mkString

  def part2(msgs: List[String]): String =
    msgs
      .transpose
      .map(fewestOccurrences)
      .mkString


  def mostOccurrences(l: List[Char]): Char =
    (l.groupBy(identity)
      .map { case (k, v) => (k, v.size) }
      .toList
      .maxBy(_._2))._1

  def fewestOccurrences(l: List[Char]): Char =
    (l.groupBy(identity)
      .map { case (k, v) => (k, v.size) }
      .toList
      .minBy(_._2))._1

  def readFile(f: String): List[String] =
    io.Source.fromFile(f).getLines().toList

  def inputFile = "data/Day06.txt"
}
