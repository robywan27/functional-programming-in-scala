import examples.chapter3.List.foldRight
import examples.chapter3.{Cons, List, Nil}

def doubleToString(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))