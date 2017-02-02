package cracking_the_coding_interview.data_structures.arrays_left_rotation

object Solution {

  def main(args: Array[String]): Unit = {

    import io.StdIn.readLine

    val Array(size, rotations) = readLine().toString.split(" ").map(_.toInt)
    val in = readLine().toString.split(" ").map(_.toInt)
    val out = new Array[Int](size)

    for (i <- in.indices) {
      val step = i - rotations
      val j = if (step < 0) size + step else step
      out(j) = in(i)
    }

    out.foreach(x => print(s"$x "))
  }

}