import chapters.chapter6_state.RNG._

def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra) {a =>
    map(rb) {b =>
      f(a, b)
    }
  }
  /*flatMap(ra) {a =>
    flatMap(rb) {b =>
      unit(f(a, b))
    }
  }*/