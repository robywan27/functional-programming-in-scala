import chapters.chapter3_functional_data_structures.{Cons, List, Nil}

def addLists(l: List[Int], m: List[Int]): List[Int] =
  (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
  }


val xs = List(6, 8, 9, 10, 7)
val ys = List(1, 2, 3)

addLists(xs, ys)                       // Cons(7,Cons(10,Cons(12,Nil)))
addLists(xs, List(1, 2, 3, 6, 1))      // Cons(7,Cons(10,Cons(12,Cons(16,Cons(8,Nil)))))
addLists(xs, List(1, 2, 3, 6, 1, 4))   // Cons(7,Cons(10,Cons(12,Cons(16,Cons(8,Nil)))))