import examples.chapter3.{Cons, List, Nil}

def addLists(l: List[Int], m: List[Int]): List[Int] =
  (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
  }