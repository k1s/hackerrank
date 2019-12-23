package statistics.mean_median_mode

object Solution {

  import scala.io.StdIn._

  def split() = readLine().split(" ")

  def mean(xs: Array[Double]) = xs.sum / xs.length

  def median(xs: Array[Double]) = {
    val sorted = xs.sorted
    val l = xs.length
    if (l % 2 != 0)
      sorted(l/2)
    else
      (sorted(l/2) + sorted(l/2 - 1)) / 2
  }

  def mode(xs: Array[Double]) = {
    val counter = xs.groupBy(identity).mapValues(_.length)
    val max = counter.maxBy(_._2)
    counter.filter(_._2 == max._2).minBy(_._1)._1.toInt
  }

  def main(args: Array[String]): Unit = {

    val n = readInt()
    val xs = split().map(_.toDouble)
    println(mean(xs))
    println(median(xs))
    println(mode(xs))

  }

}
