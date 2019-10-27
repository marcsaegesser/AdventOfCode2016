package advent

import Math._
import collection.immutable.SortedSet

object Day20 {

  def part1(input: List[Region]): Long = {
    val blackouts = applyBlackouts(input)
    val first = blackouts.head
    if(first.start > 0) 0
    else                first.end+1
  }

  def part2(input: List[Region]): Long = {
    val blackouts = applyBlackouts(input)
    val maxInt = Int.MaxValue.toLong * 2 +1
    maxInt - blackouts.map(_.size).sum + 1
  }

  case class Region(start: Long, end: Long) {
    def overlaps(other: Region): Boolean =
      other.start <= end && other.end >= start

    def merge(other: Region): Region =
      Region(min(start, other.start), max(end, other.end))

    def size: Long = end - start + 1
  }

  implicit val regionOrdering = Ordering.by((r: Region) => r.start)

  def findOverlaps(r: Region, rs: SortedSet[Region]): SortedSet[Region] =
    rs.filter(_.overlaps(r))

  def mergeOverlaps(r: Region, rs: SortedSet[Region]): Region =
    rs.foldLeft(r) { case (a, r) => a.merge(r) }

  def applyBlackouts(blackouts: List[Region]): SortedSet[Region] = {
    def helper(accum: SortedSet[Region], rest: List[Region]): SortedSet[Region] =
      rest match {
        case Nil    => accum
        case h :: t =>
          val overlaps = findOverlaps(h, accum)
          helper(accum -- overlaps + mergeOverlaps(h, overlaps), t)
      }

    helper(SortedSet.empty[Region], blackouts)
  }

  val regionRegex = """(\d+)-(\d+)""".r
  def parseRegion(l: String): Region =
    l match {
      case regionRegex(s, e) => Region(s.toLong, e.toLong)
    }

  def readFile(f: String): List[Region] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseRegion)
      .toList

  val puzzleInput = "data/Day20.txt"
  val testInput = "data/Day20-small.txt"
}
