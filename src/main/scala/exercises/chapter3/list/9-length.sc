import chapters.chapter3_functional_data_structures.List
import List.foldRight

def length[A](l: List[A]): Int =
  foldRight(l, 0)((_, acc) => 1 + acc)


length(List(1, 4, 7, 3, 2, 6))   // 6