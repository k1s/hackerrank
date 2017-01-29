object Solution {

  import io.StdIn._

  def is(s: String) = {
    val lefts = "{(["
    val rights = "})]"
    @scala.annotation.tailrec
    def yes(ss: List[Char], acc: List[Char]): Boolean = (ss, acc) match {
      case (Nil, Nil) => true
      case (head::tail, xs) if lefts.contains(head) =>
        yes(tail, head +: xs)
      case (head::tail, headx::tailx) if rights.indexOf(head) == lefts.indexOf(headx) =>
        yes(tail, tailx)
      case _ => false
    }
    if (yes(s.toList, List[Char]()))
      "YES"
    else
      "NO"
  }

  def main(args: Array[String]): Unit = {
    val end = readInt()
    (1 to end).foreach(i => println(is(readLine())))
  }

}