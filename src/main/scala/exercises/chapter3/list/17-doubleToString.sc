import chapters.chapter3_functional_data_structures.{Cons, List, Nil}
import List.foldRight

def doubleToString(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))


doubleToString(List(1.2, 3.4, 5.6))    // Cons(1.2,Cons(3.4,Cons(5.6,Nil)))