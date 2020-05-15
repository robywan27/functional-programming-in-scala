import examples.chapter3_functional_data_structures.{Cons, List, Nil}

@scala.annotation.tailrec
def foldLeft[A, B](as: List[A], v: B)(f: (B, A) => B): B =
  as match {
    case Nil => v
    case Cons(x, xs) => foldLeft(xs, f(v, x))(f)
  }


println(foldLeft(List(1, 2, 3), 0)(_ + _))    // 6