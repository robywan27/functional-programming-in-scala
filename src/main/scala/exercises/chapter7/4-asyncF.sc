import chapters.chapter7_parallelism.Par.Par
import chapters.chapter7_parallelism.Par._

def asyncF[A,B](f: A => B): A => Par[B] =
  a => lazyUnit(f(a))