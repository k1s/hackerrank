package cracking_the_coding_interview.techniques.bit_manipulation_lonely_integer

object Solution {

  import io.StdIn._

  def main(args: Array[String]): Unit = {
    readLine()
    val out = readLine().split(" ").map(_.toInt).reduce(_ ^ _)
    println(out)
  }

}