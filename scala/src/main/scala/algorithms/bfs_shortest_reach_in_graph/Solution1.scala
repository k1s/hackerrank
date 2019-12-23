package algorithms.bfs_shortest_reach_in_graph

/**
  *
  *
  */
object Solution1 {

  import scala.io.StdIn._
  import scala.collection.mutable

  type Vertex = Int

  type Graph = mutable.Map[Vertex, Set[Vertex]]

  def newGraph = mutable.Map[Vertex, Set[Vertex]]()

  def split() = readLine().split(" ").map(_.toInt)

  def shortestDistance(g: Graph, from: Vertex, search: Vertex): Int = {
    @scala.annotation.tailrec
    def bfs(g: Graph, search: Vertex, from: Vertex, elems: Set[Vertex], visited: Set[Vertex], acc: Int): Int = {
      if (elems.isEmpty)
        0
      else if (elems.contains(search))
        acc + 6
      else
        bfs(g, search, from, elems.flatMap(g.get).flatten.diff(visited), visited ++ elems, acc + 6)
    }
    bfs(g, search, from, g(from), Set(from), 0)
  }

  def start(nodesNum: Int, g: Graph, search: Vertex) = {
    def keyLength(k: Vertex) = {
      val path = shortestDistance(g, search, k)
      if (path > 0) path
      else -1
    }
    val paths = (1 to nodesNum).filter(_ != search).map(keyLength)
    val ans = paths.mkString(" ")
    println(ans)
  }

  def addEdge(g: Graph, u: Vertex, v: Vertex) = {
    val contains = g.get(u)
    if (contains.isDefined) {
      val adjs = contains.get
      g.put(u, adjs + v)
    } else
      g.put(u, Set(v))
  }

  def main(args: Array[String]): Unit = {

    val queries = readInt()

    (0 until queries).foreach(i => {

      val graph = newGraph

      val Array(nodesNum, edgesNum) = split()

      (0 until edgesNum).foreach(i => {

        val Array(u, v) = split()
        addEdge(graph, u, v)
        addEdge(graph, v, u)

      })

      start(nodesNum, graph, readInt())

    })

  }

}
