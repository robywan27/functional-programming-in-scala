trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(x: A => A, y: A => A) = x andThen y
  def zero = a => a
}


val double: Int => Int = n => n * 2
val triple: Int => Int = n => n * 3
endoMonoid.zero(1)
endoMonoid.op(double, triple)(2)


// andThen associativity
double.andThen(triple).andThen(double)(2)
double.andThen(triple.andThen(double))(2)