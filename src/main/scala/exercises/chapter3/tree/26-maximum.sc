import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def maximum(t: Tree[Int]): Int =
  t match {
    case Leaf(v) => v
    case Branch(l , r) => maximum(l).max(maximum(r))
  }