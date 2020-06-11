trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}
val intAddition = new Monoid[Int] {
  def op(x: Int, y: Int) = x + y
  def zero = 0
}

def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldRight(m.zero)((a, as) => m.op(f(a), as))


foldMap(List("2", "3", "6", "10"), intAddition)(s => s.toInt)   // 21
