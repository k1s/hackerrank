

object Solution {

  def solution(a: Array[Int]): Int = {
    import scala.collection.mutable

    val sorted = a.sortWith((i1, i2) => i2 < i1)

    println(sorted.toList)

    sorted.take(3).product
  }

}