import chapters.chapter8_property_testing.Gen
import chapters.chapter8_property_testing.Gen._

def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = /*{
  val g1Prob = g1._2 / (g1._2 + g2._2)
  val g2Prob = g2._2 / (g1._2 + g2._2)
  boolean.flatMap(_ => if (g1Prob))
}*/???