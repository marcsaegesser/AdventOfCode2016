package advent

object Day15 {

  def day15(): Unit = {
    println(s"Day15.part1 = ${part1()}")
    println(s"Day15.part2 = ${part2()}")
  }
  def part1(): Int =
    t1(finder(List(n2s, n3s, n4s, n5s, n6s)))

  def part2(): Int =
    t1(finder(List(n2s, n3s, n4s, n5s, n6s, n7s)))

  def t1(n: Int) = n*13 + 1

  val n2s: LazyList[Int] = 4  #:: (4+5)   #:: n2s.tail.map(_ + 5)
  val n3s: LazyList[Int] = 59 #:: (59+17) #:: n3s.tail.map(_ + 17)
  val n4s: LazyList[Int] = 76 #:: (76+3)  #:: n4s.tail.map(_+3)
  val n5s: LazyList[Int] = 85 #:: (85+7)  #:: n5s.tail.map(_+7)
  val n6s: LazyList[Int] = 99 #:: (99+19) #:: n6s.tail.map(_+19)
  val n7s: LazyList[Int] = 194 #:: (194+11) #:: n7s.tail.map(_+11)

  def d1(t: Int) = (t + 1) % 13 - 2
  def d2(t: Int) = (t + 2) % 5
  def d3(t: Int) = (t + 3) % 17 - 6
  def d4(t: Int) = (t + 4) % 3
  def d5(t: Int) = (t + 5) % 7  - 5
  def d6(t: Int) = (t + 6) % 19 - 2
  def d7(t: Int) = (t + 7) % 11

  def finder(factors: List[LazyList[Int]]): Int = {
    val s = factors.map(_.head).toSet
    if(s.size == 1) s.head
    else {
      factors.sortBy(_.head).reverse match {
        case Nil    => ???
        case h :: t => finder(h +: t.map(_.dropWhile(_ < h.head)))
      }
    }
  }

  case class Disc(n: Int, positions: Int, start: Int)

  val discRegex = """Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parseDisc(s: String): Disc =
    s match {
      case discRegex(n, p, s) => Disc(n.toInt, p.toInt, s.toInt)
    }

  def readFile(f: String): Vector[Disc] =
    io.Source.fromFile(f).getLines()
      .map(parseDisc)
      .toVector

  val puzzleInput = "data/Day15.txt"
  val testData = "data/Day15-small.txt"
}
