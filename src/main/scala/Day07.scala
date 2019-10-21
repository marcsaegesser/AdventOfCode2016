package advent

object Day07 {

  def day07(): Unit = {
    val input = readFile(inputFile)
    println(s"Day07.part1 = ${part1(input)}")
    println(s"Day07.part2 = ${part2(input)}")
  }

  def part1(ls: List[String]): Int =
    ls.filter(supportsTLS).size

  def part2(ls: List[String]): Int =
    ls.filter(supportsSSL).size


  val hyperNetRegex = """\[([a-z]+)\]""".r
  val superNetRegex = """([a-z]+)\[|\]([a-z]+)\[|\]([a-z]+)""".r
  val abbaRegex     = """(?=(([a-z])(?!\2)([a-z])\3\2))""".r
  val abaRegex      = """(?=(([a-z])(?!\2)[a-z]\2))""".r

  def supportsTLS(s: String): Boolean = {
    val hyperNets = hyperNetRegex.findAllIn(s)
    val superNets = superNetRegex.findAllIn(s)

    val hyperNetAbba = hyperNets.map(containsABBA).reduce(_ || _)
    val superNetAbba = superNets.map(containsABBA).reduce(_ || _)

    superNetAbba & !hyperNetAbba
  }

  def supportsSSL(s: String): Boolean = {
    val hyperNets = hyperNetRegex.findAllIn(s).toList
    val superNets = superNetRegex.findAllIn(s).toList

    val abas = superNets.map(findABA).flatten
    val babs = abas.map(mkBAB)
    babs.foldLeft(false) { case (accum, bab) =>
      accum ||
      (hyperNets.map(_.contains(bab)).toList match {
        case Nil => false
        case babs => babs.reduce(_ || _)
      })
    }
  }

  def containsABBA(s: String): Boolean =
    !abbaRegex
      .findAllMatchIn(s)
      .map(_.group(1))
      .isEmpty

  def findABA(s: String): List[String] =
    abaRegex
      .findAllMatchIn(s)
      .map(_.group(1))
      .toList

  def mkBAB(aba: String): String =
    List(aba(1), aba(0), aba(1)).mkString

  def readFile(f: String): List[String] =
    io.Source.fromFile(f).getLines().toList

  val inputFile = "data/Day07.txt"

  val testData = List("aba[bab]xyz", "xyx[xyx]xyx", "aaa[kek]eke", "zazbz[bzb]cdb")
}
