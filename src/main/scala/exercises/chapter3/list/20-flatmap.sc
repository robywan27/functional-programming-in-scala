import examples.chapter3_functional_data_structures._
import List._

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(as, List[B]()) ((x, xs) => append(f(x), xs))

flatMap(List(1,2,3))(i => List(i,i))     // Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))