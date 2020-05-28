import examples.chapter5_laziness.Stream
import examples.chapter5_laziness.Stream.cons

def fibs: Stream[Int] = {
  def loop(m: Int, n: Int): Stream[Int] =
    cons(m, loop(n, m + n))
  loop(0, 1)
}


fibs.take(10).toList