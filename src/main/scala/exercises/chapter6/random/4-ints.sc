import chapters.chapter6_state.RNG

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  @scala.annotation.tailrec
  def loop(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
    if (count == 0) (acc, rng)
    else {
      val (n, r) = rng.nextInt
      loop(count - 1, n :: acc, r)
    }
  loop(count, List[Int](), rng)
}