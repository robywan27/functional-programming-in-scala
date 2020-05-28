import chapters.chapter4_error_handling._

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(a => b.map(b => f(a, b)))
  /*(a, b) match {
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }*/



map2(Some(2), Some(3))(_ + _)                    // Some(5)
map2(Some(2), None)((x: Int, y: Int) => x + y)   // None