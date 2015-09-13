import scala.annotation.tailrec

//Won't be using the provided implementation of List

val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

def sum(t: List[Int]) = t.foldRight(0)(_ + _)

/**
 * Exercise 1
 * What will the result of the following match expression be?
 * (15)
 **/
val x = List(1, 2, 3, 4, 5) match {
  case x :: 2 :: 4 :: Nil => x
  case Nil => 42
  case x :: y :: 3 :: 4 :: Nil => x + y
  case h :: t => h + sum(t)
  case _ => 101
}
x

/**
 * Exercise 2
 * Implement the function tail for "removing" the first element
 * of a List
 **/
def tail2[A](as: List[A]) = if (as.isEmpty) Nil else as.drop(1)
/**
 * Exercise 3
 * Generalize tail to the function drop , which removes the first
 * n elements from a list.
 **/
def drop2[A](as: List[A], n: Int): List[A] = if (n == 0) as else drop2(as.tail, n - 1)

/**
 * Exercise 4
 * Implement dropWhile , 10 which removes elements from the
 * List prefix as long as they match a predicate.
 **/
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Nil => l
  case h :: t if (f(h)) => dropWhile(t)(f)
  case h :: t if (!f(h)) => l
}

/**
 * Exercise 5
 * implement the function setHead for
 * replacing the first element of a List with a different value.
 **/
def setHead2[A](l: List[A], newHead: A) = l match {
  case head :: tail => newHead :: tail
  case _ => l
}
setHead2(list, 800)

/**
 * Exercise 6
 * Implement a function,
 * init , which returns a List consisting of all but the last element of a List . So,
 * given List(1,2,3,4) , init will return List(1,2,3)
 **/
def init2[A](l: List[A]): List[A] = l match {
  case Nil => l
  case h :: t :: Nil => h :: Nil
  case h :: t => h :: init2(t)
}
init2(List(1, 2, 3, 4))

/**
 * Exercise 7 1.a
 * sum implemented with foldRight
 **/
def sumInts(ints: List[Int]): Int = ints.foldRight(0)(_ + _)

/**
 * Exercise 7 1.b
 * product implemented with foldRight
 **/
def productInts(ints: List[Int]): Int = ints.foldRight(1)(_ * _)

/**
 * Exercise 9
 * length of a list with foldLeft
 **/
def length2[A](l: List[A]): Int = l.foldLeft(0)((acc, elem) => 1 + acc)
length2(List(1, 2, 3, 4, 5, 6, 7, 8))


/**
 * Exercise 10
 * foldRight is not tail-recursive and will StackOverflow
 * for large lists. Convince yourself that this is the case, then write another general
 * list-recursion function, foldLeft that is tail-recursive
 */
@tailrec
def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  case Nil => z
  case head::tail => foldLeft2(tail,f(z,head))(f)
}

/**
 * Exercise 11
 * Write sum , product , and a function to compute the
 * length of a list using foldLeft
 * */
def sumWithFold(l:List[Int]) = foldLeft2(l,0)(_+_)
def productWithFold(l:List[Int]) = foldLeft2(l,1)(_*_)
def lengthWithFold(l:List[Int]) = foldLeft2(l,0)((acc,list)=>acc+1)
lengthWithFold(List(1,2,3,4,5,6))

/**
 * Exercise 12
 * Write a function that returns the reverse of a list (so given
 * List(1,2,3) it returns List(3,2,1) ). See if you can write it using a fold.
 * */
def reverse2[A](l:List[A]):List[A]= l match{
  case h::t => reverse2(t):::List(h)
  case Nil => Nil
}


/**
 * Exercise 18
 * Write a function map , that generalizes modifying each element
 * in a list while maintaining the structure of the list
 **/
def map2[A, B](l: List[A])(f: A => B): List[B] = l match {
  case head :: tail => f(head) :: map2(tail)(f)
  case _ => Nil
}
map2(List(1, 2, 3, 4, 5, 6))(x => x * 2)


/**
 * Exercise 19
 * Write a function filter that removes elements from a list
 * unless they satisfy a given predicate
 **/
def filter2[A](l: List[A], pred: A => Boolean): List[A] = l match {
  case Nil => l
  case h :: t if pred(h) => filter2(t, pred)
  case h :: t if !pred(h) => h :: filter2(t, pred)
}
filter2(list, (a: Int) => a % 2 == 0)



