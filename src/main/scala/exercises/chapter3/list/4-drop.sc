import chapters.chapter3_functional_data_structures.{Cons, List, Nil}

@scala.annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] =
  if (n <= 0)   l
  else l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n - 1)
  }


drop(List(1, 2, 3, 4, 5, 6), 3)    // Cons(4,Cons(5,Cons(6,Nil)))