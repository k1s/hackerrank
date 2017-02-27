package src

import scala.collection.immutable.Seq

/**
  * Current exercise
  */
object Solution {

  import scala.io.StdIn._

  def process(n: Int) = {

    def isSafePosition(desk: Stream[Int], row: Int, column: Int) = {
      def isSafe(queenRow: Int, queenColumn: Int): Boolean = {
        import Math.abs
        if (queenColumn == column || queenRow == row ||
          row - column == queenRow - queenColumn ||
          row + column == queenRow + queenColumn ||
          (abs(queenRow - row) == 2 && abs(queenColumn - column) == 1) ||
          (abs(queenRow - row) == 1 && abs(queenColumn - column) == 2))
          false
        else
          true
      }

      desk.zipWithIndex.forall { case (queen, index) => isSafe(index, queen) }
    }

    def iterate(n: Int, row: Int, desk: Stream[Int]): Stream[Stream[Int]] = {
      if (row == 0)
        Stream(desk)
      else {
        Stream.range(0, n).filter(isSafePosition(desk, row, _)).flatMap(x => iterate(n, row - 1, x +: desk))
      }
    }

    iterate(n, n, Stream()).size
  }

  def main(args: Array[String]): Unit = {

    val n = readInt()

    println(process(n))

  }

  val stdinString = "11\n"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)

}
