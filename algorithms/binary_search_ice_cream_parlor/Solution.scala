package algorithms.binary_search_ice_cream_parlor

object Solution {

  import io.StdIn._

  def process(m: Int, costs: List[Int], n: Int, map: Map[Int, Int]): Unit = costs match {
    case Nil =>
    case x::xs =>
      val search = m - x
      val op = map.get(search)
      if (op.isDefined) {
        val index = op.get
        if (index < n)
          println(index + " " + n)
        else
          println(n + " " + index)
      } else
        process(m, xs, n + 1, map + (x -> n))
  }

  def main(args: Array[String]): Unit = {
    val end = readInt()
    (1 to end).foreach(i => {
      val m = readInt()
      readInt()
      process(m, readLine().split(" ").map(_.toInt).toList, 1, Map[Int, Int]())
    })
  }

}
