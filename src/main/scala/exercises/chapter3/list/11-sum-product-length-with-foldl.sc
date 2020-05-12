import examples.chapter3_functional_data_structures.List
import examples.chapter3_functional_data_structures.List.foldLeft

def sumL(ns: List[Int]): Int =
  foldLeft(ns, 0)(_ + _)
def productL(ds: List[Double]): Double =
  foldLeft(ds, 1.0)(_ * _)
def lengthL[A](l: List[A]): Int =
  foldLeft(l, 0)((acc, _) => acc + 1)