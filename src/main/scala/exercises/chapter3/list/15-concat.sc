import chapters.chapter3_functional_data_structures._
import List._

def concat[A](xss: List[List[A]]): List[A] =
  foldRight(xss, List[A]()) (append)
  /*xss match {
    case Nil => Nil
    case Cons(xs, xss) => append(xs, concat(xss))
  }*/


concat(List(List(1, 2, 3), List(4, 5), List(6, 7, 8, 9)))   // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Cons(8,Cons(9,Nil)))))))))
