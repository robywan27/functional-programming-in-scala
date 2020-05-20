import examples.chapter3_functional_data_structures.{Cons, List, Nil}
import List.foldRight

def map[A, B](l: List[A])(f: A => B): List[B] =
  foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))


// stack-safe implementation
def map2[A, B](l: List[A])(f: A => B): List[B] = {
  val buf = new collection.mutable.ListBuffer[B]
  @scala.annotation.tailrec
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(x, xs) =>
      buf += f(x)
      go(xs)
  }

  go(l)
  // converting from the standard Scala list to the list we've defined here
  List(buf.toList: _*)
}



map(List(3, 5, 2, 8, 9))(x => x * x)   // Cons(9,Cons(25,Cons(4,Cons(64,Cons(81,Nil)))))
map(List(3, 5, 2, 8, 9))(_ * 2)          // Cons(6,Cons(10,Cons(4,Cons(16,Cons(18,Nil)))))