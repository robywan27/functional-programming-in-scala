import examples.chapter3.{Cons, List}
import examples.chapter3.List.foldRight

def appendRL[A](l: List[A], m: List[A]): List[A] =
// foldRight(l, m)((x, acc) => Cons(x, acc))
  foldRight(l, m)(Cons(_, _))