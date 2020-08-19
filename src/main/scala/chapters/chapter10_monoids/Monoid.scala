package chapters.chapter10_monoids

import chapters.chapter8_property_testing.{Gen, Prop}

trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String): String = x + y
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(x: List[A], y: List[A]): List[A] = x ++ y
    def zero: List[A] = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(x: A => A, y: A => A) = x andThen y
    def zero = a => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, as) => m.op(f(a), as))

  def foldRightWithFoldMap[A](a: A)(f: (A, A) => A): A =
    ???

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???


}
