trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}
val intAddition = new Monoid[Int] {
  def op(x: Int, y: Int) = x + y
  def zero = 0
}


def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  v match {
    case IndexedSeq() => m.zero
    case IndexedSeq(x) => m.op(f(x), foldMapV(v.tail, m)(f))
    case _ =>
      val (left, right) = v.tail.splitAt(v.length / 2)
      m.op(f(v(0)), m.op(foldMapV(left, m)(f), foldMapV(right, m)(f)))
  }



foldMapV(IndexedSeq("7", "2", "11", "3"), intAddition)(_.toInt)
