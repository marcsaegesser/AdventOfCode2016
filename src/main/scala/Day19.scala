package advent

import collection.immutable._

object Day19 {

  def day19(): Unit = {
    println(s"Day19.part1 = ${part1(puzzleInput)}")
    println(s"Day19.part2 = ${part2(puzzleInput)}")
  }

  def part1(elves: SortedSet[Int]): Int = {
    if(elves.size == 1) elves.head
    else {
      val next = elves.sliding(2, 2).map(_.take(1)).reduce(_++_)
      if(elves.size % 2 == 0) play1(next)
      else                   play1(next.drop(1))
    }
  }

  def part2(start: SortedSet[Int]): Int = {
    def helper(elves: SortedSet[Int], curr: Int): Int =
      if(elves.size == 1) elves.head
      else {
        val size = elves.size
        val split = (curr + size/2)%size
        val (a, b) = elves.splitAt(split)
        if(split > curr) helper(a ++ b.tail, (curr+1)%(size-1))
        else             helper(a ++ b.tail, curr%(size-1))
      }

    helper(start, 0)
  }

  val puzzleInput = 3001330
}
