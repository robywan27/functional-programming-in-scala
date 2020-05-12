import examples.chapter3_functional_data_structures.List
import examples.chapter3_functional_data_structures.List.foldRight

def length[A](l: List[A]): Int =
  foldRight(l, 0)((_, acc) => 1 + acc)
/*l match {
  case Nil => 0
  case Cons(x, xs) => 1 + length(xs)
}*/