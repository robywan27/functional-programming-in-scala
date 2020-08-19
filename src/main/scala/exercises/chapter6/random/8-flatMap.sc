import chapters.chapter6_state.RNG.Rand
import chapters.chapter6_state.RNG._

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }