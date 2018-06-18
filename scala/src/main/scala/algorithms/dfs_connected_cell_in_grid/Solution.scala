package algorithms.dfs_connected_cell_in_grid

/**
  *
  *
  */
object Solution {

  import scala.io.StdIn._
  import scala.collection.immutable.Seq
  import scala.collection.mutable

  def split() = readLine().split(" ").map(_.toInt)

  val visited = new mutable.HashSet[(Int, Int)]()

  def adjs(matrix: Array[Array[Int]], row: Int, column: Int): Seq[(Int, Int)] = {
    def cell(row: Int, column: Int) =
      if ((row >= 0 && row <= matrix.length - 1) && (column >= 0 && column <= matrix(0).length - 1))
        Some((row, column))
      else
        None
    val hl = cell(row - 1, column - 1)
    val h = cell(row - 1, column)
    val hr = cell(row - 1, column + 1)
    val ml = cell(row, column - 1)
    val mr = cell(row, column + 1)
    val dl = cell(row + 1, column - 1)
    val d = cell(row + 1, column)
    val dr = cell(row + 1, column + 1)
    List(hl, h, hr, ml, mr, dl, d, dr).flatten.filter { case (r, c) => !visited.contains((r, c)) && matrix(r)(c) != 0 }
  }

  def count(matrix: Array[Array[Int]], row: Int, column: Int): Int = {
    visited.add((row, column))
    val as = adjs(matrix, row, column)
    if (as.isEmpty)
      1
    else {
      as.foreach(t => visited.add(t))
      1 + as.map { case (r, c) => count(matrix, r, c) }.sum
    }
  }

  def process(matrix: Array[Array[Int]]) = {
    val sums = for {
      row <- matrix.indices
      column <- matrix(0).indices
    } yield {
      if (matrix(row)(column) == 0 || visited.contains(row, column)) 0 else count(matrix, row, column)
    }
    println(sums.max)
  }

  def main(args: Array[String]): Unit = {

    val rows = readInt()
    val columns = readInt()
    val matrix: Array[Array[Int]] = Array.ofDim[Int](rows, columns)
    (0 until rows).foreach(row => matrix(row) = split())
    process(matrix)

  }

}
