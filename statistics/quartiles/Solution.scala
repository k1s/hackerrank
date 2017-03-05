package statistics.quartiles

object Solution {

  import scala.io.StdIn._

  def quartiles(xs: Array[Double]): List[Double] = {
    val sorted = xs.sorted
    val m = median(sorted)
    val l = sorted.length
    val left = sorted.take(l/2)
    val right = sorted.drop(if (l % 2 == 0) l/2 else l/2+1)
    List(median(left), m, median(right))
  }

  def median(xs: Array[Double]) = {
    val sorted = xs.sorted
    val l = xs.length
    if (l % 2 != 0)
      sorted(l/2)
    else
      (sorted(l/2) + sorted(l/2 - 1)) / 2
  }

  def split() = readLine().split(" ")

  def main(args: Array[String]): Unit = {

    val n = readInt()

    quartiles(split().map(_.toDouble)).foreach(println)

  }

}
