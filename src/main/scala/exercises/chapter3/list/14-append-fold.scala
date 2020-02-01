import examples.chapter3_functional_data_structures.{Cons, List}
import examples.chapter3_functional_data_structures.List.foldRight

def appendRL[A](l: List[A], m: List[A]): List[A] =
// foldRight(l, m)((x, acc) => Cons(x, acc))
  foldRight(l, m)(Cons(_, _))