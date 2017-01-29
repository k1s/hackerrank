object Solution {

  import io.StdIn._

  def isPrime(n: Int) = {
    @scala.annotation.tailrec
    def yes(n: Int, nums: Stream[Int], sqrt: Double): Boolean = nums match {
      case head#::tail if sqrt < head => true
      case head#::tail if (n % head) == 0 => false
      case head#::tail => yes(n, tail.filter(_ % head != 0), sqrt)
    }
    if (n == 1 || !(n != 1 && yes(n, Stream.from(2), Math.sqrt(n))))
      "Not prime"
    else
      "Prime"
  }

  def main(args: Array[String]): Unit = {
    val end = readInt()
    (1 to end).foreach(i => println(isPrime(readInt())))
  }

}