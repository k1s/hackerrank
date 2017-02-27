package src
import scala.collection.immutable.Seq

/**
  * Current exercise
  */
object Solution {

  import scala.io.StdIn._

  def process(n: Int) = {

    def isSafePosition(desk: List[Int], row: Int, column: Int) = {
      def isSafe(queenRow: Int, queenColumn: Int): Boolean = {
        import Math.abs
        if (queenColumn == column || queenRow == row ||
          abs(row - column) == abs(queenRow - queenColumn) ||
          (row + column) == (queenRow + queenColumn) ||
          (abs(queenRow - row) == 2 && abs(queenColumn - column) == 1) ||
          (abs(queenRow - row) == 1 && abs(queenColumn - column) == 2))
          false
        else
          true
      }

      desk.zipWithIndex.forall { case (queen, index) => isSafe(index, queen) }
    }

    def iterate(n: Int, row: Int, desk: List[Int]): Seq[List[Int]] = {
      if (row == n)
        desk
      else {
        val r = for {
          column <- 0 to n if isSafePosition(desk, row, column)
          xs <- iterate(n, row + 1, desk :+ column)
        } yield xs
        println(s"desk $desk")
        println(s"r $r")
        r
      }
  }


    iterate(n, 0, List())

  }

  def main(args: Array[String]): Unit = {

    val n = readInt()

    println(process(n))

  }

  val stdinString = "4\n"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)

}
