/**
 * Write a function to find all the combinations of three numbers
 * that sum to zero
 * Sample Imput [2, 3, 1, -2, -1, 0, 2, -3, 0]
 * Sample Output [2,-2,0]
 * Sample Output [3,-2,-1]
 * Sample Output [3,0,-3]
 * */
def sumZero(l: List[Int]) = {
  for {
    num1 <- l
    num2 <- l
    num3 <- l
    if (num1 + num2 + num3) == 0
  } yield s"$num1 + $num2 + $num3 = 0"
}
sumZero(List(2, 3, 1, -2, -1, 0, 2, -3, 0)).foreach(println)

/**
 * Input - array of integers size N, integer Threshold
 * Output - the number of pairs (x,y) of distinct elements with condition
 * x + y <= Threshold
 * */
def tupleThreshold(l:List[Int], threshold:Int):List[(Int,Int)]={
  for{
    x <- l
    y <- l
    if ((x+y)<=threshold && x!=y)
  } yield (x,y)
}
tupleThreshold(List(0,1,2,3),3).foreach(println)

/**
 * Design an algorithm to find all the common elements in two sorted lists of numbers
 * Input l1: 2,5,5,5 l2: 2,2,3,5,5,7
 * Output: 2,5,5
 * */
def commonElementsInList(l1:List[Int], l2:List[Int]):List[Int]={
  for{
    e1 <- l1
    if(l2.contains(e1))
  } yield e1
}
commonElementsInList(List(2,5,5,5),List(2,2,3,5,5,7)).foreach(println)


