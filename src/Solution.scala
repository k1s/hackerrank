/**
  * Current exercise
  */
object Solution {

  import scala.language.postfixOps

  sealed abstract class Trie {
    def isEmpty: Boolean
    def +(key: List[Char]): Trie
    def keys: List[String]
    def keysStarts(prefix: List[Char]): List[String]
  }
  case object Empty extends Trie {

    override def isEmpty: Boolean = true

    override def +(key: List[Char]): Trie = key match {
      case Nil => Empty
      case List(x) => Node(x, Empty, Empty, Empty)
      case h::t => Node(h, Empty, Empty + t, Empty)
    }

    override def toString: String = ""

    override def keys: List[String] = List()

    override def keysStarts(prefix: List[Char]): List[String] = List()

  }

  case class Node[+A](char: Char, left: Trie, mid: Trie, right: Trie) extends Trie {

    override def isEmpty: Boolean = false

    override def +[B >: A](key: String, value: B): Trie[B] = key.length match {
      case 0 => this
      case _ =>
        if (key.head < this.char) this.copy(left = this.left + (key, value))
        else if (key.head > this.char) this.copy(right = this.right + (key, value))
        else if (key.length == 1) this.copy(value = Some(value))
        else this.copy(mid = this.mid + (key.tail, value))
    }

    override def adj: List[Trie[A]] = List(this.left, this.mid, this.right)

    override def toString: String = this.char + " " + this.value + " "

    override def size: Int = 1 + this.left.size + this.mid.size + this.right.size

    override def keys: List[String] = collect(this, "", List())

    override def keysStarts(prefix: String): List[String] = getNode(this, prefix) match {
      case None => List()
      case Some(Node(c, v, l, m, r)) => v match {
        case None => collect(m, prefix, List())
        case Some(x) => collect(m, prefix, List(prefix))
      }
    }

        private def collect[B >: A](trie: Trie[B], prefix: String, acc: List[String]): List[String] = trie match {
      case Empty => acc
      case Node(c, v, l, m, r) =>
        def sides = collect(l, prefix, acc) ++ collect(r, prefix, acc)
        if (v.nonEmpty) sides ++ collect(m, prefix :+ c, (prefix :+ c) :: acc) distinct
        else sides ++ collect(m, prefix :+ c, acc) distinct
    }

    private def getNode[B](trie: Trie[B], key: String): Option[Node[B]] = trie match {
      case Empty => None
      case Node(c, v, l, m, r) =>
        if (key.length == 0) None
        else if (key.head < c) getNode(l, key)
        else if (key.head > c) getNode(r, key)
        else if (key.length == 1) Some(Node(c, v, l, m, r))
        else getNode(m, key.tail)
    }

  }

  import scala.io.StdIn._

  def split() = readLine().split(" ").map(_.toInt)

  def process(s: String) = ???

  def main(args: Array[String]): Unit = {

    val rows = readInt()
    (0 until rows).foreach(row => process(readLine()))

  }

  //  val stdinString = "3\n4\n1 1 0 0\n0 1 1 0\n0 0 1 0\n"//1 0 0 0"
  val stdinString = "4\n4\n1 1 0 0\n0 1 1 0\n0 0 1 0\n1 0 0 0"

  System.setIn(new java.io.ByteArrayInputStream(stdinString.getBytes("UTF-8")))
  Solution.main(null)

}
