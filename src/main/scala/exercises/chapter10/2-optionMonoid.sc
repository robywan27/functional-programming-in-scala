trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def op(x: Option[A], y: Option[A]) = x orElse y
  def zero = None
}


optionMonoid.zero
optionMonoid.op(Some(5), Some(3))
optionMonoid.op(None, Some(3))


// orElse
Some(3).orElse(Some(4)).orElse(Some(6))
// prove associative property
None.orElse(None).orElse(Some(9))
None.orElse(None.orElse(Some(9)))
