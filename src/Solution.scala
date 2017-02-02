/**
  * Current exercise
  */
object Solution {

  class Heap() {

      val low = new collection.mutable.PriorityQueue[Int]()(implicitly[Ordering[Int]].reverse)
      val high = new collection.mutable.PriorityQueue[Int]()

      def add(x: Int) = {
        val destination = if (low.size <= high.size) low else high
        destination.enqueue(x)
        merge()
      }

      def merge() = {
        while(low.nonEmpty && high.nonEmpty && low.max > high.max) {
          val lowHead= low.dequeue()
          val highHead = high.dequeue()
          low.enqueue(highHead)
          high.enqueue(lowHead)
        }
      }

      def median() = if (low.size == high.size) (low.max + high.max) / 2.0 else low.max.asInstanceOf[Double]

    }

  import scala.io.StdIn._

  var heap = new Heap()

  def split() = readLine().split(" ").map(_.toInt)

  def process(n: Int) = {
    heap.add(n)
    println(heap.median())
  }

  def main(args: Array[String]): Unit = {

    val end = readInt()
    (1 to end).foreach(i => process(readInt()))

  }

  val stdinString = "6\n12\n4\n5\n3\n8\n7"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)

}
