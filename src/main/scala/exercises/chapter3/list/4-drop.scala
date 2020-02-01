import examples.chapter3_functional_data_structures.List.tail
import examples.chapter3_functional_data_structures.{Cons, List, Nil}

@scala.annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] =
  if (n <= 0)   l
  else l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n - 1)
  }

// Solution without pattern matching
def drop2[A](l: List[A], n: Int): List[A] = {
  @annotation.tailrec
  def loop(xs: List[A], n: Int): List[A] =
    if (n == 0)     xs
    else if (xs == Nil)   Nil
    else loop(tail(xs), n - 1)

  loop(l, n)
}