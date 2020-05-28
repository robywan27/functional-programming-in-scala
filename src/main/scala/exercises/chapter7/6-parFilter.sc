import chapters.chapter7_parallelism.Par._

def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
  // This solution is not good because it does not parallelize the logic of the
  // function, i.e. the filtering logic
  sequence(as.filter(f).map(unit))
