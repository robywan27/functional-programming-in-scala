def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  val n = as.length
  @annotation.tailrec
  def looper(i: Int): Boolean = {
    if (i >= n - 1)   true
    else if (!ordered(as(i), as(i + 1)))   false
    else looper(i + 1)
  }

  looper(0)
}