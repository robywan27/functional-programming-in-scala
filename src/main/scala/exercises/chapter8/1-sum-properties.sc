// sum: List[Int] => Int

/*
  Properties
*/

// 1. val prop = forAll(intList)(ns => ns.sum == ns.reverse.sum) - sum of reverse of a list = sum list
// 2. List(1, 1, 1, 1).sum = 4*1 - sum of list with all equal elements = length of list by element
// 3. List(1, 2, 3, 4, 5).sum = 5*6/2 - a series of consecutive numbers starting from 1 = n*(n+1) / 2
// 4. List(1, 10, 43, -3).sum = List(-3, 1, 10, 43) - sum of a list is equals to sum of that list sorted out
// 5. List().sum = 0
// 6. List(5).sum = 5 - sum of list of 1 element = the element
// 7. List(1, 2, 3, 4, 5, 6).sum = List(1, 2).sum + List(3, 4, 5, 6).sum - associative property
