object Solution {

  import io.StdIn._

  def counter(s: String): Map[Char, Int] = s.toList.groupBy(identity).mapValues(_.size)

  def count(c: Char, m1: Map[Char, Int], m2: Map[Char, Int]): Int = {
    val v1 = m1.getOrElse(c, 0)
    val v2 = m2.getOrElse(c, 0)
    (v1 max v2) - (v1 min v2)
  }

  def main(args: Array[String]): Unit = {
    val s1 = readLine()
    val s2 = readLine()

    val m1 = counter(s1)
    val m2 = counter(s2)
    val out = ('a' to 'z').map(count(_, m1, m2)).sum

    println(out)
  }

}