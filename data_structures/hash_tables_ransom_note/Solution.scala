package data_structures.hash_tables_ransom_note

object Solution {

  import io.StdIn._

  def counter(s: String) = s.split(" ").groupBy(identity).mapValues(_.length)

  def main(args: Array[String]): Unit = {
    readLine()
    val m1 = counter(readLine())
    val m2 = counter(readLine())
    val out = if (m2.forall(t => m1(t._1) >= t._2)) "Yes" else "No"

    println(out)
  }

}