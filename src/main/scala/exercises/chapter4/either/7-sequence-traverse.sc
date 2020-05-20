import examples.chapter4_error_handling._

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldLeft[Either[E, List[B]]] (Right(Nil)) ((xs, x) => f(x).map2(xs)(_ :: _))

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  traverse(es)(a => a)


sequence(List(Right(3), Right(5), Right(9)))                                                    // Right(List(9, 5, 3))
sequence(List(Right(3), Right(5), Left("error"), Right(9), Left("wrong")))                      // Left(wrong)
traverse(List(3, 5, 9))((x: Int) => try Right(x.toFloat) catch {case e: Exception => Left(e)})  // Right(List(9.0, 5.0, 3.0))