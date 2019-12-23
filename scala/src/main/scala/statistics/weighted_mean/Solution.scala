package statistics.weighted_mean

object Solution {

  import scala.io.StdIn._

  def split() = readLine().split(" ")

  def printRound1(d: Double) = println(f"$d%.1f")

  def weightedMean(xs: Array[Double], ws: Array[Double]) = (xs, ws).zipped.map(_ * _).sum / ws.sum

  def main(args: Array[String]): Unit = {

    val n = readInt()
    val xs = split().map(_.toDouble)
    val ws = split().map(_.toDouble)
    printRound1(weightedMean(xs, ws))

  }

}
