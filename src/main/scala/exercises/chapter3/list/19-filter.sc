import examples.chapter3_functional_data_structures.{Cons, List, Nil}
import List.foldRight

def filter[A](l: List[A])(p: A => Boolean): List[A] =
  foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)


// stack-safe implementation
def filter2[A](l: List[A])(p: A => Boolean): List[A] = {
  val buf = new collection.mutable.ListBuffer[A]
  @scala.annotation.tailrec
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(x, xs) =>
      if (p(x))  buf += x
      go(xs)
  }

  go(l)
  // converting from the standard Scala list to the list we've defined here
  List(buf.toList: _*)
}



println(filter(List(4, 6, 8, 3, 1, 6, 5))(_ % 2 != 0))  // Cons(3,Cons(1,Cons(5,Nil)))