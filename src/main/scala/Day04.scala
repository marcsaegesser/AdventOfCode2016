package advent

object Day04 {

  def part1(infos: List[RoomInfo]): Int =
    infos.filter(verifyChecksum).map(_.sectorId).sum

  def part2(infos: List[RoomInfo]): Int =
    infos
      .filter(verifyChecksum)
      .map(i => (i.sectorId, decryptRoomName(i)))
      .filter(_._2 == "northpole object storage")
      .map(_._1)
      .head


  def verifyChecksum(roomInfo: RoomInfo): Boolean =
    roomInfo.checksum == computeChecksum(roomInfo.name)

  def decryptRoomName(info: RoomInfo): String = {
    info.name.toSeq.map {
      case '-' => ' '
      case c   => (((c - 'a') + info.sectorId%26) % 26 + 'a').toChar
    }.mkString
  }

  case class RoomInfo(name: String, sectorId: Int, checksum: String)

  implicit val customOrdering = new Ordering[(Char, Int)] {
    def compare(a: (Char, Int), b: (Char, Int)) = {
      val byInt = b._2 compare a._2
      if(byInt != 0) byInt
      else          a._1 compare b._1
    }
  }

  def computeChecksum(name: String): String =
    name.toSeq
      .filterNot(_ == '-')
      .groupBy(identity)
      .map { case (k, v) => (k, v.size) }
      .toVector
      .sorted
      .take(5)
      .map(_._1)
      .mkString

  val roomInfoRegex = """([a-z-]+)(\d+)\[([a-z]+)\]""".r

  def parseRoomInfo(s: String): RoomInfo =
    s match {
      case roomInfoRegex(n, s, c) => RoomInfo(n.init, s.toInt, c)
    }

  def readFile(f: String): List[RoomInfo] =
    io.Source.fromFile(f).getLines()
      .map(parseRoomInfo)
      .toList

  val inputFile = "data/Day04.txt"
}
