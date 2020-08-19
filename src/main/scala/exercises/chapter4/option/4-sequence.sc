import chapters.chapter4_error_handling._
import Option.map2

def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldLeft[Option[List[A]]] (Some(Nil)) ((xs, x) => map2(x, xs)(_ :: _))
  /*a match {
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
  }*/


sequence(List(Some("string"), Some("hello"), Some("something")))  // Some(List(something, hello, string))
sequence(List(Some("string"), None, Some("hello")))               // None