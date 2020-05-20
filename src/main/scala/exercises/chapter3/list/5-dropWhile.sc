import examples.chapter3_functional_data_structures.{Cons, List}

@scala.annotation.tailrec
def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
  l match {
    // pattern guard: use if <cond> after the pattern
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _ => l
  }

dropWhile(List(6, 8, 9, 10, 7), (x: Int) => x % 2 == 0)  // Cons(9,Cons(10,Cons(7,Nil)))