import chapters.chapter3_functional_data_structures._
import List._

// What will be the result of the following match expression?
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

// Answer: 3 case is matched, with x = 1 and y = 2; the expression then returns 3