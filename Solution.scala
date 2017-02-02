/**
  * Current exercise
  */
object Solution {

  import scala.io.StdIn._

  def split() = readLine().split(" ").map(_.toInt)

  def process(n: Int, coins: List[Int], acc: Int): Int = (n, coins) match {
    case (x, h::Nil) if x % h == 0 => acc + 1
    case (x, h::Nil) => 0
    case (x, h :: t) if x == h =>
      acc + 1
    case (x, h :: t) if x % h == 0 =>
      process(x - h, t, acc + 1)
  }

  def main(args: Array[String]): Unit = {

    val Array(n, _) = split()
    println(process(n, split().toList, 0))

  }

  val stdinString = "4 3\n1 4"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)

}
