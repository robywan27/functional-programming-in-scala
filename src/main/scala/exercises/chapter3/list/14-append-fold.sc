import examples.chapter3_functional_data_structures.{Cons, List}
import List.foldRight

def appendFold[A](l: List[A], m: List[A]): List[A] =
  foldRight(l, m)(Cons(_, _))


appendFold(List(1, 2, 3), List(4, 5, 6, 7))   // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Nil)))))))