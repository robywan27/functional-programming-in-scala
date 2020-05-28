import chapters.chapter6_state.RNG

def nonNegativeInt2(rng: RNG): (Int, RNG) = {
  val (nextInt, nextRng) = rng.nextInt
  val nextPositiveInt = nextInt match {
    case n if n < 0 => nextInt * (-1)
    case n if n == Int.MinValue => 0
    case _ => nextInt
  }
  (nextPositiveInt, nextRng)
}

// more concise solution
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}