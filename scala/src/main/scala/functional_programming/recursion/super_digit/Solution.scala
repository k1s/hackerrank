package src

object Solution {

  import scala.io.StdIn._

  implicit class BigIntOps(x: BigInt) {
    def toDigits = x.toString.map(_.asDigit)
  }

  @scala.annotation.tailrec
  def superDigit(x: BigInt): Long = x.toDigits match {
    case Seq(y) => y
    case ys @ _ => superDigit(ys.sum)
  }

  def ints() = readLine().split(" ").map(BigInt(_))

  def main(args: Array[String]): Unit = {

    val Array(n, k) = ints()

    val nSum = superDigit(n.toDigits.sum)

    print(superDigit(k * nSum))

  }

}

