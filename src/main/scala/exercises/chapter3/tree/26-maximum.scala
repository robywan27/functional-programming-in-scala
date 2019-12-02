import examples.chapter3.{Branch, Leaf, Tree}

def maximum(t: Tree[Int]): Int =
  t match {
    case Leaf(v) => v
    case Branch(l , r) => maximum(l).max(maximum(r))
  }