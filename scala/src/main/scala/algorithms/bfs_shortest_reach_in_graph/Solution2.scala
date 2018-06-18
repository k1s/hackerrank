package algorithms.bfs_shortest_reach_in_graph

/**
  *
  *
  */
object Solution2 {

  import scala.io.StdIn._
  import scala.collection.mutable

  type Graph = Array[mutable.Set[Int]]

  def newGraph(n: Int) = Array.fill(n + 1)(mutable.Set[Int]())

  def split() = readLine().split(" ").map(_.toInt)

  def shortestDistance(g: Graph, nodesNum: Int, search: Int): Array[Int] = {
    val nextToVisit = new mutable.Queue[Int]()
    val visited = new mutable.HashSet[Int]()
    var distances = new Array[Int](nodesNum + 1)
    nextToVisit.enqueue(search)
    while (nextToVisit.nonEmpty) {
      var next = nextToVisit.dequeue()
      var adjs = g(next)
      for (x <- adjs) {
        if (!visited.contains(x)) {
          visited.add(x)
          nextToVisit.enqueue(x)
          distances(x) = distances(next) + 6
        }
      }
    }
    distances
  }

  def start(nodesNum: Int, g: Graph, search: Int) = {
    val paths = shortestDistance(g, nodesNum, search)
    println(paths.zipWithIndex.filter { case (v, i) => i != search }.drop(1).map { case (x, i) => if (x == 0) -1 else x }.mkString(" "))
  }

  def addEdge(g: Graph, u: Int, v: Int) = {
    g(u).add(v)
    g(v).add(u)
  }

  def main(args: Array[String]): Unit = {

    val queries = readInt()

    (0 until queries).foreach(i => {

      val Array(nodesNum, edgesNum) = split()

      val graph = newGraph(nodesNum)

      (0 until edgesNum).foreach(i => {

        val Array(u, v) = split()
        addEdge(graph, u, v)

      })

      start(nodesNum, graph, readInt())

    })

  }

}
