package advent

import java.security.MessageDigest

object Day05 {

  def part1(s: String): String = {
    LazyList.from(0)
      .map(s+_)
      .map(md5HashString)
      .filter(_.startsWith("00000"))
      .take(8)
      .map(_.drop(5).take(1))
      .mkString
  }

  def part2(s: String): String = {
    def helper(accum: Vector[Char], i: Long): String = {
      if(!accum.contains(' ')) accum.mkString
      else {
        val hash = md5HashString(s+i)
        if(hash.startsWith("00000")) {
          val index = hash.drop(5).head - '0'
          if(index < 8 && accum(index) == ' ') helper(accum.updated(index, hash.drop(6).head), i+1)
          else                               helper(accum, i+1)
        } else {
          helper(accum, i+1)
        }
      }
    }

    helper(Vector.fill(8)(' '), 0)
  }

  def md5HashString(s: String) = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map(b => f"$b%02x")
      .mkString
  }

  val puzzleInput = "reyedfim"
}
