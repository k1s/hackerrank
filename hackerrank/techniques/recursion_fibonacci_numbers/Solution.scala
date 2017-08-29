package techniques.recursion_fibonacci_numbers

object Solution {

  import io.StdIn._

  val fibs: Stream[BigInt] = 0 #:: 1 #:: fibs.zip(fibs.tail).map(t => t._1 + t._2)

  def main(args: Array[String]): Unit = {

    val in = readInt()

    println(fibs(in))

  }

}
