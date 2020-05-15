import examples.chapter3_functional_data_structures.{Cons, List, Nil}

def setHead[A](l: List[A], y: A): List[A] =
  l match {
    case Nil => sys.error("cannot modify head of empty list")
    case Cons(_, xs) => Cons(y, xs)
  }

val xs = List(6, 8, 9, 10, 7)
println(setHead(xs, 4))   // Cons(4,Cons(8,Cons(9,Cons(10,Cons(7,Nil)))))
println(xs)   // xs remains unchanged - Cons(6,Cons(8,Cons(9,Cons(10,Cons(7,Nil)))))