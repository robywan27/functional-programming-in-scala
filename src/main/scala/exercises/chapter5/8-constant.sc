import chapters.chapter5_laziness.Stream
import chapters.chapter5_laziness.Stream.cons

def constant[A](a: A): Stream[A] = cons(a, constant(a))
  // more efficient
  /*{
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }*/


constant(5).take(7).toList
constant("ciao").take(5).toList
constant(20).map(_ / 2).take(3).toList