import chapters.chapter5_laziness.Stream
import Stream._

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case None => empty
    case Some(t) => cons(t._1, unfold(t._2)(f))
  }

lazy val onesWithUnfold: Stream[Int] =
  unfold(1)(one => Some(one, one))

def constantWithUnfold[A](a: A): Stream[A] =
  unfold(a)(a => Some(a, a))

def fromWithUnfold(n: Int): Stream[Int] =
  unfold(0)(n => Some(n + 1, n + 1))

def fibsWithUnfold: Stream[Int] =
  unfold((0, 1)) {case (m, n) => Some(m, (n, m + n))}




onesWithUnfold.take(10).toList
constantWithUnfold('c').take(10).toList
fromWithUnfold(2).take(10).toList
fibsWithUnfold.take(10).toList