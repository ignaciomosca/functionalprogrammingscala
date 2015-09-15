import scala.annotation.tailrec

/**
 * Exercise 1
 * Fibonacci sequence
 * @param n -> fibonacci of this number
 * */
def fib(n:Int):Int = {
  /**
   * Tail recursive function that implements the fibonacci sequence
   * @param n -> fibonacci of this number
   * @param previous -> previous number in the sequence, default is 0 because fib(0)=0
   * @param next -> next number in the sequence, default is 1 because fib(1)=1
   */
  @tailrec
  def go(n:Int,previous:Int,next:Int):Int={
    if(n==0) previous
    else go(n-1,next,next+previous)
  }
  go(n,0,1)
}

//test cases
fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)
fib(8)

/**
 * Exercise 2
 * isSorted checks whether an Array[A] is sorted according to a given comparison function.
 * @param as -> array to determine if it's sorted
 * @param gt -> comparison function
 * */
def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  if(as.isEmpty) true else gt(as.apply(0),as.apply(1)) && isSorted(as.drop(2),gt)
}


/**
 * Exercise 3
 * partial1 , takes a value and a function of two arguments and returns a function
 * of one argument as its result. The name comes from the fact that the function
 * is being applied to some but not all of its required arguments
 * @param a -> argument a
 * @param f -> function to which I partially apply a
 * */
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)

/**
 * Exercise 4
 * currying converts a function of N arguments into a function of one argument that
 * returns another function as its result
 * @param f -> function to curry
 * */
def curry[A,B,C](f: (A, B) => C): A => (B => C) = a=>b=>f(a,b)

/**
 * Exercise 5
 * uncurry reverses the transformation of curry .
 * Note that since => associates to the right, A => (B => C) can be written as:
 * A => B => C
 * @param f -> function to uncurry
 * */
def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

/**
 * Exercise 6
 * compose is a higher-order function that composes two functions
 * @param f -> function to compose with
 * @param g -> function to compose with
 * */
def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

