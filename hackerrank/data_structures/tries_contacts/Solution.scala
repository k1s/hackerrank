package data_structures.tries_contacts

object Solution {

  import scala.language.postfixOps

  sealed abstract class Trie {

    def +(key: List[Char]): Trie

    def countStartWith(prefix: List[Char]): Int

    def apply(prefix: List[Char]): Int = countStartWith(prefix)

  }

  case object Empty extends Trie {

    override def +(key: List[Char]): Trie = key match {
      case Nil => Empty
      case List(x) => Node(x, true, Empty, Empty, Empty)
      case h :: t => Node(h, false, Empty, Empty + t, Empty)
    }

    override def countStartWith(prefix: List[Char]): Int = 0

  }

  case class Node(char: Char, isEnd: Boolean, left: Trie, mid: Trie, right: Trie) extends Trie {

    override def +(key: List[Char]): Trie = key match {
      case Nil => this
      case h :: t if h < this.char => this.copy(left = this.left + key)
      case h :: t if h > this.char => this.copy(right = this.right + key)
      case h :: Nil => this.copy(isEnd = true)
      case h :: t => this.copy(mid = this.mid + t)
    }

    override def countStartWith(prefix: List[Char]): Int = getNode(this, prefix) match {
      case Node(c, end, l, m, r) =>
        if (end)
          collect(m, 1)
        else
          collect(m, 0)
      case _ => 0
    }

    private def collect(trie: Trie, acc: Int): Int = trie match {
      case Empty => acc
      case Node(c, end, l, m, r) =>
        def sides = collect(l, 0) + collect(r, 0)
        if (end)
          sides + collect(m, acc + 1)
        else
          sides + collect(m, acc)
    }

    @scala.annotation.tailrec
    private def getNode(trie: Trie, key: List[Char]): Trie = (trie, key) match {
      case (Empty, _) => Empty
      case (_, Nil) => Empty
      case (Node(c, v, l, m, r), h :: t) if h < c => getNode(l, key)
      case (Node(c, v, l, m, r), h :: t) if h > c => getNode(r, key)
      case (x, h :: Nil) => x
      case (Node(c, v, l, m, r), h :: t) => getNode(m, t)
    }

  }

  import scala.io.StdIn._

  def split() = readLine().split(" ")

  var trie: Trie = Empty

  def process(s: Array[String]) = s match {
    case Array("add", x) => trie = trie + x.toList
    case Array("find", x) => println(trie(x.toList))
  }

  def main(args: Array[String]): Unit = {

    val rows = readInt()
    (0 until rows).foreach(row => process(split()))

  }

}