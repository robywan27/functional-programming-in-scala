import examples.chapter3_functional_data_structures.{Cons, List, Nil}

def zipWith[A](l: List[A], m: List[A])(f: (A, A) => A): List[A] =
  (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }