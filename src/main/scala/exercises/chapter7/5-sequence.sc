import examples.chapter7_parallelism.Par.Par
import examples.chapter7_parallelism.Par._

def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  ???