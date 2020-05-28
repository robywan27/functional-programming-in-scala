import chapters.chapter3_functional_data_structures.{Cons, List, Nil}
import List.foldRight

def increment(l: List[Int]): List[Int] =
  foldRight(l, Nil: List[Int])((x, acc) => Cons(1 + x, acc))


increment(List(4, 2, 1))   // Cons(5,Cons(3,Cons(2,Nil)))