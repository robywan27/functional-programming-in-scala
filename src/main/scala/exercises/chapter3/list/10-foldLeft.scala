import examples.chapter3.{Cons, List, Nil}

@scala.annotation.tailrec
def foldLeft[A, B](as: List[A], v: B)(f: (B, A) => B): B =
  as match {
    case Nil => v
    case Cons(x, xs) => foldLeft(xs, f(v, x))(f)
  }