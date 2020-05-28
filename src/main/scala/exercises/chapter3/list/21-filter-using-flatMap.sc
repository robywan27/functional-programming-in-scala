import examples.chapter3_functional_data_structures._
import List.flatMap

def filterWithFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
  flatMap(l)(x => if (p(x)) List(x) else Nil)



filterWithFlatMap(List(4, 6, 8, 3, 1, 6, 5))(_ % 2 != 0)  // Cons(3,Cons(1,Cons(5,Nil)))