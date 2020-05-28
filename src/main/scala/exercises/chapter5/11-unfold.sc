import chapters.chapter5_laziness.Stream
import Stream._

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }


unfold(0)((n: Int) => if (n == 10) None else Some(n + 1, n + 1)).toList
