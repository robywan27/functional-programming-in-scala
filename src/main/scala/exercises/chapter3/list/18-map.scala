import examples.chapter3.List.foldRight
import examples.chapter3.{Cons, List, Nil}

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