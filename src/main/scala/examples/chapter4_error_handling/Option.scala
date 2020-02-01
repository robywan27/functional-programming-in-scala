package examples.chapter4_error_handling

sealed trait Option[+A] {
  // maps the the value in Some
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  // like map but f maps to Option type, so you can apply it directly to the value in Some
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  // retrieves this option object if it is defined, otherwise get the default option object
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _ => this
    }

  // unwraps the Option type by getting either the value in Some or a default value
  def getOrElse[B >: A](default: => B): B =   // >: means B is a supertype of A; default is not evaluated unless needed
    this match {
      case None => default
      case Some(a) => a
    }

  // converts Some values to None if predicate is false
  def filter(p: A => Boolean): Option[A] =
    this match {
      case Some(a) if p(a) => this
      case _ => None
    }
}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)   None
    else Some(xs.sum / xs.length)

  /*def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => math.pow(x - m, 2))*/

  def main(args: Array[String]): Unit = {
    println(mean(Seq()))                      // None
    println(mean(Seq(1.0, 2.0, 0.0, 5.0)))    // Some(2.0)

  }
}