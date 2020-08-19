import chapters.chapter3_functional_data_structures._
import List.foldRight

foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))  // Cons(1,Cons(2,Cons(3,Nil))) - same list as input