package src

import scala.collection.immutable.Seq

object Solution {

  import scala.io.StdIn._

  def process(n: Int) = {

    def isSafePosition(desk: List[Int], row: Int, column: Int) = {
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

      desk.zipWithIndex.forall{case (queen, index) => isSafe(index, queen)}
    }

    def iterate(n: Int, row: Int, desk: List[Int]): Seq[Seq[Int]] = {
      if (row == n)
        Seq(desk)
      else
        for {
          column <- 0 until n if isSafePosition(desk, row, column)
          xs <- iterate(n, row + 1, desk :+ column)
        } yield xs
    }

    iterate(n, 0, List()).size
  }

  def main(args: Array[String]): Unit = {

    val n = readInt()

    println(process(n))

  }

}
