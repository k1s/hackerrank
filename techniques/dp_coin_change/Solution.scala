package techniques.dp_coin_change

object Solution {

  import scala.io.StdIn._

  def split() = readLine().split(" ").map(_.toInt)

  def process(n: Int, coins: List[Int]): Long = {
    val amounts = new Array[Long](n + 1)
    amounts(0) = 1
    for (coin <- coins)
      for (x <- coin to n)
        amounts(x) += amounts(x - coin)
    amounts(n)
  }

  def main(args: Array[String]): Unit = {

    val Array(n, _) = split()
    println(process(n, split().toList))

  }

}