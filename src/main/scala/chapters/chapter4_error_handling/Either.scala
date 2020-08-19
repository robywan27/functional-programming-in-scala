package chapters.chapter4_error_handling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(_) => this
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
//    this.flatMap(aa => b.map(bb => f(aa, bb)))
}


case class Left[+E] (value: E) extends Either[E, Nothing]
case class Right[+A] (value: A) extends Either[Nothing, A]



object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("Mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft[Either[E, List[B]]] (Right(Nil)) ((xs, x) => f(x).map2(xs)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)


  def main(args: Array[String]): Unit = {
    val possiblySuccessfulValue = Right(5)
    val failedValue: Either[String, Int] = Left("Something went wrong")

    println("map")
    println(possiblySuccessfulValue.map(_ * 2))   // Right(10)
    println(failedValue.map(_ * 2))               // Left(Something went wrong)
    println()

    println("flatMap")
    println(possiblySuccessfulValue.flatMap((x: Int) => if (x < 0) Right(x * 2) else Left("Could not double")))   // Left(Could not double)
    println(failedValue.flatMap((x: Int) => if (x > 0) Right(x * 2) else Left("Could not double")))               // Left(Something went wrong)
    println()

    println("orElse")
    println(possiblySuccessfulValue.orElse(Right(8)))   // Right(5)
    println(failedValue.orElse(Right(8)))               // Right(8)
    println()

    println("map2")
    println(possiblySuccessfulValue.map2(Right(7))(_ + _))  // Right(12)
    println()
  }
}
