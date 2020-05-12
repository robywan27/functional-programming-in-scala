import examples.chapter3_functional_data_structures.{Cons, List, Nil}

def setHead[A](l: List[A], y: A): List[A] =
  l match {
    case Nil => sys.error("cannot modify head of empty list")
    case Cons(_, xs) => Cons(y, xs)
  }