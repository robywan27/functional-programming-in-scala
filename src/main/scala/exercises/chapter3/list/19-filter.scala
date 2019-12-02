import examples.chapter3.List.foldRight
import examples.chapter3.{Cons, List, Nil}

def filter[A](l: List[A])(p: A => Boolean): List[A] =
  foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)
/*l match {
  case Nil => Nil
  case Cons(x, xs) =>
    if (p(x))   Cons(x, filter(xs)(p))
    else filter(xs)(p)
}*/


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