trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldRight(m.zero)((a, as) => m.op(f(a), as))


def foldRightWithFoldMap[A](a: A)(f: (A, A) => A): A =
  ???

