package advent

import java.security.MessageDigest

object Day14 {

  def day14(): Unit = {
    println(s"Day14.part1 = ${part1(puzzleInput)}")
    println(s"Day14.part2 = ${part2(puzzleInput)}")
  }

  def part1(salt: String): Int =
    findKeys(hashes(salt), 64).head._1

  def part2(salt: String): Int =
    findKeys(hashes2(salt), 64).head._1

  def hashes(salt: String): LazyList[(Int, String)] = {
    LazyList.from(0).map(i => (i, md5HashString(salt + i)))
  }

  def doHash(h: String, n: Int): String =
    if(n == 0) h
    else      doHash(md5HashString(h), n-1)

  def hashes2(salt: String): LazyList[(Int, String)] =
    LazyList.from(0).map(i => (i, doHash(md5HashString(salt + i), 2016)))

  val tripleRegex = """([0-9a-f])\1\1""".r

  def findKeys(hashes: LazyList[(Int, String)], n: Int): List[(Int, String)] = {
    def verify(triple: String, hs: LazyList[(Int, String)]): Boolean = {
      val search = (triple+triple).take(5)
      hs.take(1000).exists { case (i, h) => h.contains(search) }
    }
    def helper(accum: List[(Int, String)], hs: LazyList[(Int, String)]): List[(Int, String)] =
      if(accum.size == n) accum
      else
        hs match {
          case h #:: t =>
            tripleRegex.findFirstIn(h._2) match {
              case Some(s) if(verify(s, t)) => helper(h +: accum, t)
              case Some(s)                  => helper(accum, t)
              case None                     => helper(accum, t)
            }
        }

    helper(List(), hashes)
  }

  def md5HashString(s: String) = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map(b => f"$b%02x")
      .mkString
  }

  val puzzleInput = "yjdafjpo"
}
