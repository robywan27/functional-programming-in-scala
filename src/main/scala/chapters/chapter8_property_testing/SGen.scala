package chapters.chapter8_property_testing

import chapters.chapter8_property_testing.Gen._

//case class SGen[+A](forSize: Int => Gen[A]) {
case class SGen[A](forSize: Int => Gen[A]) {

}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => listOfN(size, g))
}
