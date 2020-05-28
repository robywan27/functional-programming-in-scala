import examples.chapter6_state.RNG._

def doubleWithMap: Rand[Double] =
  map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))