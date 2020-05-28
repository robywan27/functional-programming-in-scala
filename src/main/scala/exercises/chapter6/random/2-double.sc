import examples.chapter6_state.RNG

def double(rng: RNG): (Double, RNG) = {
  val (newInt, newRng) = rng.nextInt
  (newInt.toDouble / (Int.MaxValue + 1), newRng)
}