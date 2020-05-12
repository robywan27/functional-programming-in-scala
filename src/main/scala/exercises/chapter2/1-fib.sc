def fibIter(n: Int): Int = {
  @annotation.tailrec
  def fibHelper(n: Int, firstVal: Int, secondVal: Int): Int =
    if (n == 0)   firstVal
    else fibHelper(n - 1, secondVal, firstVal + secondVal)

  fibHelper(n, 0, 1)
}

// recursive algorithm - cannot do tail call optimization
def fibRec(n: Int): Int =
  if (n == 0 || n == 1)   n
  else fibRec(n - 1) + fibRec(n - 2)
