import examples.chapter3_functional_data_structures.{Cons, List, Nil}

def init[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("cannot take init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

// stack-safe alternative version with internal buffer to avoid stacking up stacks at each recursive call.
// Notice that mutation is used to alter the content of the buffer, but this is fine because it is just an implementation
// detail internal to this method; the list passed as input is not touched
def init2[A](l: List[A]): List[A] = {
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]
  @annotation.tailrec
  def go(cur: List[A]): List[A] =
    cur match {
      case Nil => sys.error("cannot take init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(x, xs) =>
        buf += x
        go(xs)
    }

  go(l)
}


init(List(1, 2, 3, 4, 5))  // Cons(1,Cons(2,Cons(3,Cons(4,Nil))))