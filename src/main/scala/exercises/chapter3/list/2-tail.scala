import examples.chapter3.{Cons, List, Nil}

def tail[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("cannot take tail of empty list")
    case Cons(_, xs) => xs
  }