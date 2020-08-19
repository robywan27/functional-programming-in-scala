import chapters.chapter6_state.RNG._

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, ra2) = ra(rng)
    val (b, rb2) = rb(ra2)
    (f(a, b), rb2)
  }