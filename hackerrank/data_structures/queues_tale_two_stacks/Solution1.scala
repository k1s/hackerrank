package data_structures.queues_tale_two_stacks

/**
  *
  *
  */
object Solution1 {

  import io.StdIn._

  class Queue(in: List[Int], out: List[Int]) {

    def enqueue(x: Int) = new Queue(x :: in, out)

    def dequeue() = out match {
      case Nil if in.nonEmpty =>
        val reverse = in.reverse
        (reverse.head, new Queue(Nil, reverse.tail))
      case x::xs => (x, new Queue(in, xs))
    }

  }

  @scala.annotation.tailrec
  def process(xs: List[List[Int]], queue: Queue): Unit = xs match {
    case Nil =>
    case List(1, x)::tail => process(tail, queue.enqueue(x))
    case List(2)::tail => process(tail, queue.dequeue()._2)
    case List(3)::tail => println(queue.dequeue()._1); process(tail, queue)
  }

  def main(args: Array[String]): Unit = {
    val end = readInt()
    val input = (1 to end).map(i => readLine().split(" ").map(_.toInt).toList).toList
    process(input, new Queue(List(), List()))
  }

}
