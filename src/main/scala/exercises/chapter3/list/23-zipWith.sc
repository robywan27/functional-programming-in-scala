import chapters.chapter3_functional_data_structures.{Cons, List, Nil}

def zipWith[A](l: List[A], m: List[A])(f: (A, A) => A): List[A] =
  (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }


zipWith(List(6, 8, 9, 10, 7), List(1, 2, 3))(_ + _)     // Cons(7,Cons(10,Cons(12,Nil)))
zipWith(List("hola", "hello"), List("chico", "world"))(_.concat(_))    // Cons(holachico,Cons(helloworld,Nil))