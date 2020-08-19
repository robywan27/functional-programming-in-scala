import chapters.chapter3_functional_data_structures.{Cons, List}
import List._

/*
With Scala List library
  List(1,2,3).foldRight(List[Int]())((x, xs) => xs ++ List(x))
  List(1,2,3).foldLeft(List[Int]())((xs, x) => x :: xs)
*/

def reverse[A](l: List[A]): List[A] =
  foldLeft(l, List[A]()) ((xs, x) => Cons(x, xs))

def reverseR[A](l: List[A]): List[A] =
  foldRight(l, List[A]()) ((x, xs) => append(xs, List(x)))


reverse(List(3, 6, 8, 9, 1))   // Cons(1,Cons(9,Cons(8,Cons(6,Cons(3,Nil)))))