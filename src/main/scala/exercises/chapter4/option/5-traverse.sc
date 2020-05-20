import examples.chapter4_error_handling._
import Option._

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldLeft[Option[List[B]]] (Some(Nil)) ((xs, x) => map2(f(x), xs)(_ :: _))
  /*a match {
    case Nil => Some(Nil)
    case x :: xs => f(x).flatMap(xx => traverse(xs)(f).map(xx :: _))
  }*/

  // Less efficient implementation - needs 2 scans of the list
//  sequence(a map f)


def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(a => a)



traverse(List(1, 7, 3, 5))((x: Int) => if (x > 0) Some(x.toDouble) else None)    // Some(List(5.0, 3.0, 7.0, 1.0))
traverse(List(1, 7, -3, 5))((x: Int) => if (x > 0) Some(x.toDouble) else None)   // None
sequenceViaTraverse(List(Some("string"), Some("hello"), Some("something")))      // Some(List(something, hello, string))