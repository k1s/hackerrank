object Things extends App {

  def missingInteger(a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val (min, max, doneSet) = a.foldLeft((1, 1, Set[Int]())) { case ((min, max, set), next) =>
      (Math.min(min, next), Math.max(max, next), set + next)
    }

    val miss = (min to max).find(i => !doneSet.contains(i))

    if (miss.isEmpty)
      max + 1
    else if (miss.get <= 0)
      1
    else
      miss.get
  }

}