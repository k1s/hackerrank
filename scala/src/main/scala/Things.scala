object Things extends App {

  def missingInteger(a: Array[Int]): Int = {
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

  def missingInteger2(a: Array[Int]): Int = {
    val (min, max, doneSet) = a.foldLeft((1, 1, Set[Int]())) { case ((min, max, set), next) =>
      (Math.min(min, next), Math.max(max, next), set + next)
    }
    val miss = (min to max).toSet.diff(doneSet)

    if (miss.isEmpty)
      max + 1
    else if (miss.head <= 0)
      1
    else
      miss.head
  }

}