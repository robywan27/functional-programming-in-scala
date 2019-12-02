import examples.chapter3.List.foldRight
import examples.chapter3.{Cons, List, Nil}

def increment(l: List[Int]): List[Int] =
  foldRight(l, Nil: List[Int])((x, acc) => Cons(1 + x, acc))
/*l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(x + 1, increment(xs))
}*/