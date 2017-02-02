package cracking_the_coding_interview.techniques.recursion_davis_staircase

object Solution {

  import scala.io.StdIn._

  def process(n: Int) = {
    @scala.annotation.tailrec
    def helper(n: Int, next: Int, acc1: Int, acc2: Int, acc3: Int): Int = {
      if (n == next)
        acc3
      else
        helper(n, next + 1, acc2, acc3, acc1 + acc2 + acc3)
    }

    if (n == 1)
      1
    else if (n == 2)
      2
    else if (n == 3)
      4
    else
      helper(n, 3, 1, 2, 4)
  }

  def main(args: Array[String]): Unit = {

    val end = readInt()

    (1 to end).foreach(i => {
      println(process(readInt()))
    })

  }

}
