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
