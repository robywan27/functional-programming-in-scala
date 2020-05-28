import chapters.chapter3_functional_data_structures.{Cons, List, Nil}

def tail[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("cannot take tail of empty list")
    case Cons(_, xs) => xs
  }


tail(List(6, 8, 9, 10, 7))   // Cons(8, Cons(9, Cons(10, Cons(7, Nil))))
tail(Nil)                    // error: cannot take tail of empty list