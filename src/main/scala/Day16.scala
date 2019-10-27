package advent

object Day16 {

  def day16(): Unit = {
    println(s"Day16.part1 = ${solve(puzzleInput, diskSize)}")
    println(s"Day16.part2 = ${solve(puzzleInput, diskSize2)}")
  }

  def solve(input: String, size: Int): String =
    checksum(dragon(input, size))

  def invert(s: String): String =
    s.map {
      case '0' => '1'
      case '1' => '0'
      case _   => ???
    }

  def dragon(a: String, length: Int): String = {
    if(a.length >= length) a.take(length)
    else                  dragon(a + '0' + invert(a.reverse), length)
  }

  def checksum(input: String): String = {
    val chksum =
      input.toSeq.sliding(2, 2).
        map { s =>
          s.toString match {
            case "00" => "1"
            case "11" => "1"
            case _    => "0"
          }
        }
        .mkString

    if(chksum.length % 2 == 0) checksum(chksum)
    else                      chksum
  }

  val diskSize = 272
  val diskSize2 = 35651584

  val puzzleInput = "01111001100111011"

  val testData1 = "10000"
}
