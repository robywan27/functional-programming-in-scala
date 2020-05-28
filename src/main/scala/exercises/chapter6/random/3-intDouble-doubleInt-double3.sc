import chapters.chapter6_state.RNG

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (i, r) = rng.nextInt
  val (d, r2) = RNG.double(r)
  ((i, d), r2)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val ((i, d), r) = intDouble(rng)
  ((d, i), r)
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (d, r) = RNG.double(rng)
  val (d2, r2) = RNG.double(r)
  val (d3, r3) = RNG.double(r2)
  ((d, d2, d3), r3)
}