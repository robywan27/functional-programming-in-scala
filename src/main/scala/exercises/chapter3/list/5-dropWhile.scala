import examples.chapter3_functional_data_structures.{Cons, List, Nil}

@scala.annotation.tailrec
def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
  l match {
    // pattern guard: use if <cond> after the pattern
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _ => l
  }

// solution without pattern guard
@scala.annotation.tailrec
def dropWhile2[A](l: List[A], p: A => Boolean): List[A] =
  l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (p(x))   dropWhile2(xs, p)
      else l
  }