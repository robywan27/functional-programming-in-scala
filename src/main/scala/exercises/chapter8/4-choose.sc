import chapters.chapter6_state.{RNG, State}
import chapters.chapter8_property_testing.Gen


def choose(start: Int, stopExclusive: Int): Gen[Int] =
  Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

