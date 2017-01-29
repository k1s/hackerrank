object Solution {

  import io.StdIn._

  def main(args: Array[String]): Unit = {
    readLine()
    val out = readLine().split(" ").map(_.toInt).reduce(_ ^ _)
    println(out)
  }

}