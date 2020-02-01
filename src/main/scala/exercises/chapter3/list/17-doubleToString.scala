import examples.chapter3_functional_data_structures.List.foldRight
import examples.chapter3_functional_data_structures.{Cons, List, Nil}

def doubleToString(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))