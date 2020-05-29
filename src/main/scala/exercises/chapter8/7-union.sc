import chapters.chapter8_property_testing.Gen
import chapters.chapter8_property_testing.Gen._

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
  boolean.flatMap(b => if (b) g1 else g2)