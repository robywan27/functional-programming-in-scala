import chapters.chapter5_laziness.Stream
import chapters.chapter5_laziness.Stream.cons

def from(n: Int): Stream[Int] = cons(n, from(n + 1))


from(3).take(20).toList