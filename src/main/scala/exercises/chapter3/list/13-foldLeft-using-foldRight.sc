import chapters.chapter3_functional_data_structures.{Cons, List, Nil}
import List._

def foldLeftUsingFoldRight[A, B](as: List[A], v: B)(f: (B, A) => B): B =
  foldRight (reverse(as), v) ((a: A, b: B) => f(b, a))

def foldRightUsingFoldLeft[A, B](as: List[A], v: B)(f: (A, B) => B): B =
  foldLeft (reverse(as), v) ((b: B, a: A) => f(a, b))


println(foldLeftUsingFoldRight(List(1, 2, 3), List[Int]())((xs, x) => Cons(x, xs)))    // 6
println(foldRightUsingFoldLeft(List(1, 2, 3), List[Int]())((x, xs) => Cons(x, xs)))    // 6