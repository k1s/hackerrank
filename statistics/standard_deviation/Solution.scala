package statistics.standard_deviation

object Solution {

  import scala.io.StdIn._

  def mean(xs: Array[Double]) = xs.sum / xs.length

  def doubles() = readLine().split(" ").map(_.toDouble)

  def printRound1(d: Double) = println(f"$d%.1f")

  def sqrd(x: Double, m: Double) = Math.pow(x - m, 2)

  def std(xs: Array[Double]) = {
    val m = mean(xs.sorted)
    val sqrdsSum = xs.map(sqrd(_, m)).sum
    Math.sqrt(sqrdsSum/xs.length)
  }

  def main(args: Array[String]): Unit = {

    readInt()

    val xs = doubles()

    printRound1(std(xs))

  }

}
