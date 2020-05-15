import examples.chapter3_functional_data_structures._
import List.{append, flatMap, foldRight}

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(as, List[B]()) ((x, xs) => append(f(x), xs))

def filter[A](l: List[A])(p: A => Boolean): List[A] =
   foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)