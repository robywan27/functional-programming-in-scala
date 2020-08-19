import chapters.chapter3_functional_data_structures._
import List._

/*
  Correct implementation
*/
@scala.annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  @scala.annotation.tailrec
  def startsWith(sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
      case _ => false
    }

  sup match {
    case Nil => sub == Nil
    case Cons(x, xs) if startsWith(Cons(x, xs), sub) => true
    case Cons(_, xs) => hasSubsequence(xs, sub)
  }
}


/*
  This implementation is almost correct, but fails for case when all elements in
  sublist are present in super-list but not in order, e.g. in this case:
  hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4, 5))
 */
@scala.annotation.tailrec
def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean =
  (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => hasSubsequence2(xs, ys)
    case (Cons(_, xs), Cons(y, ys)) => hasSubsequence2(xs, Cons(y, ys))
    case _ => false
  }




hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3))    // true
hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4))       // true
hasSubsequence(List(1, 2, 3, 4, 5), List(4))          // true
hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4, 5))    // false
hasSubsequence(List(1, 2, 3, 4, 5), List(2, 6))       // false
hasSubsequence(List(1, 2, 3, 4, 5), List(6))          // false