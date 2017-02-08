package cracking_the_coding_interview.data_structures.queues_tale_two_stacks

object Solution2 {

  import io.StdIn._
  import scala.collection.mutable


  class Queue() {

    var in = mutable.Stack[Int]()
    var out = mutable.Stack[Int]()

    def head() = {
      reverse()
      out.head
    }

    def enqueue(x: Int) = {
      in.push(x)
    }

    def dequeue() = {
      reverse()
      out.pop()
    }

    def reverse() = {
      if (out.isEmpty && in.nonEmpty)
        while (in.nonEmpty)
          out.push(in.pop())
    }

  }

  val queue = new Queue()

  def process(xs: List[Int]): Unit = xs match {
    case Nil =>
    case List(1, x) => queue.enqueue(x)
    case List(2) => queue.dequeue()
    case List(3) => println(queue.head())
  }

  def main(args: Array[String]): Unit = {
    val end = readInt()
    val input = (1 to end).foreach(i => process(readLine().split(" ").map(_.toInt).toList))
  }

}