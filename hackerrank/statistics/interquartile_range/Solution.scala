package statistics.interquartile_range

object Solution {

  import scala.io.StdIn._

  def median(xs: Array[Double]) = {
    val sorted = xs.sorted
    val l = xs.length
    if (l % 2 != 0)
      sorted(l/2)
    else
      (sorted(l/2) + sorted(l/2 - 1)) / 2
  }

  def quartiles(xs: Array[Double]): List[Double] = {
    val sorted = xs.sorted
    val m = median(sorted)
    val l = sorted.length
    val left = sorted.take(l/2)
    val right = sorted.drop(if (l % 2 == 0) l/2 else l/2+1)
    List(median(left), m, median(right))
  }

  def doubles() = readLine().split(" ").map(_.toDouble)

  def ints() = readLine().split(" ").map(_.toInt)

  def printRound1(d: Double) = println(f"$d%.1f")

  def quartileRange(xs: Array[Double], fs: Array[Int]) = {
    val list = (xs, fs).zipped.flatMap((x, f) => List.fill(f)(x)).sorted

    val List(q1, q2, q3) = quartiles(list)

    q3 - q1
  }

  def main(args: Array[String]): Unit = {

    readInt()

    val xs = doubles()

    val fs = ints()

    printRound1(quartileRange(xs, fs))

  }

}
