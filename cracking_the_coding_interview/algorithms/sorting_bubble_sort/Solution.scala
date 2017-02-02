package cracking_the_coding_interview.algorithms.sorting_bubble_sort

object Solution {

  def main(args: Array[String]): Unit = {

    import io.StdIn._

    readLine()
    val in = readLine().toString.split(" ").map(_.toInt)
    var numSwaps = 0

    for (i <- in.indices)
      for (j <- 0 until in.length - 1)
        if (in(j) > in(j + 1)) {
          swap(j)
          numSwaps += 1
        }

    def swap(j: Int) = {
      val tmp = in(j + 1)
      in(j + 1) = in(j)
      in(j) = tmp
    }

    println(s"Array is sorted in $numSwaps swaps.")
    println(s"First Element: ${in.head}")
    println(s"Last Element: ${in.last}")

  }

}
