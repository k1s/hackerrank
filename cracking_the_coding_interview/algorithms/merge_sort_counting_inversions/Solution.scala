package cracking_the_coding_interview.algorithms.merge_sort_counting_inversions

object Solution {

  import scala.io.StdIn._

  def process(list: Vector[Int]) = {

    def mergeSort(list: Vector[Int], acc: BigInt): (Vector[Int], BigInt) = list match {
      case Vector(x) => (Vector(x), acc)
      case xs =>
        val middle = xs.length / 2
        val (ls, rs) = xs splitAt middle
        val (leftSorted, leftCount) = mergeSort(ls, acc)
        val (rightSorted, rightCount) = mergeSort(rs, acc)
        merge(leftSorted, rightSorted, Vector(), leftCount + rightCount)
    }

    @scala.annotation.tailrec
    def merge(lefts: Vector[Int], rights: Vector[Int], accVector: Vector[Int], acc: BigInt): (Vector[Int], BigInt) = (lefts, rights) match {
      case (Vector(), rs) => (accVector ++ rs, acc)
      case (ls, Vector()) => (accVector ++ ls, acc)
      case (lh +: lt, rh +: rt) if lh <= rh => merge(lt, rh +: rt, accVector :+ lh, acc)
      case (lh +: lt, rh +: rt) if lh > rh => merge(lh +: lt, rt, accVector :+ rh, acc + lefts.length)
    }

    println(mergeSort(list, 0)._2)

  }

  def main(args: Array[String]): Unit = {

    val end = readInt()

    (1 to end).foreach(i => {
      readLine(); process(readLine().split(" ").map(_.toInt).toVector)})
  }

}