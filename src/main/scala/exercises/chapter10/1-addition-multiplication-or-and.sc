trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

val intAddition = new Monoid[Int] {
  def op(x: Int, y: Int) = x + y
  def zero = 0
}

val intMultiplication = new Monoid[Int] {
  def op(x: Int, y: Int) = x * y
  def zero = 1
}

val booleanOr = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x || y
  def zero = false
}

val booleanAnd = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x && y
  def zero = true
}


intAddition.zero
intAddition.op(3, 5)
intMultiplication.zero
intMultiplication.op(3, 5)
booleanOr.zero
booleanOr.op(true, false)
booleanAnd.zero
booleanAnd.op(true, false)
