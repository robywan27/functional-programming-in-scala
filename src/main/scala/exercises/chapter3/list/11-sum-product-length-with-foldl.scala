import examples.chapter3.List
import examples.chapter3.List.foldLeft

def sumL(ns: List[Int]): Int =
  foldLeft(ns, 0)(_ + _)
def productL(ds: List[Double]): Double =
  foldLeft(ds, 1.0)(_ * _)
def lengthL[A](l: List[A]): Int =
  foldLeft(l, 0)((acc, _) => acc + 1)