//Won't be using the provided implementation of List

val list = List(1,2,3,4,5,6,7,8,9)

def sum(t: List[Int]) = t.foldRight(0)(_+_)

val x = List(1,2,3,4,5) match {
  case x::2::4::Nil => x
  case Nil => 42
  case x::y::3::4::Nil => x + y
  case h::t => h + sum(t)
  case _ => 101
}
x

/**
 * Implement the function tail for "removing" the first element
of a List
 * */
def tail2[A](as:List[A]) = if(as.isEmpty) Nil else as.drop(1)
/**
 * Generalize tail to the function drop , which removes the first
n elements from a list.
 * */
def drop2[A](as:List[A], n:Int):List[A] = if(n==0) as else drop2(as.tail,n-1)
/**
 * Implement dropWhile , 10 which removes elements from the
List prefix as long as they match a predicate.
 * */
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Nil => l
  case h::t if(f(h)) => dropWhile(t)(f)
  case h::t if(!f(h)) => l
}

/**
 * implement the function setHead for
replacing the first element of a List with a different value.
 * */
def setHead2[A](l:List[A], newHead:A)= l match {
  case head::tail => newHead::tail
  case _ => l
}
setHead2(list,800)

/**
 * Implement a function,
init , which returns a List consisting of all but the last element of a List . So,
given List(1,2,3,4) , init will return List(1,2,3)
 * */
def init2[A](l:List[A]):List[A] = l match {
  case Nil => l
  case h::t::Nil => h::Nil
  case h::t => h::init2(t)
}
init2(List(1,2,3,4))

/**
 * sum implemented with foldRight
 * */
def sumInts(ints: List[Int]): Int = ints.foldRight(0)(_+_)

/**
 * product implemented with foldRight
 * */
def productInts(ints: List[Int]): Int = ints.foldRight(1)(_*_)

/**
 * length of a list with foldLeft
 * */
def length2[A](l:List[A]):Int = l.foldLeft(0)((acc,elem)=>1+acc)
length2(List(1,2,3,4,5,6,7,8))





def filter2[A](l:List[A],pred:A=>Boolean):List[A]= l match {
  case Nil => l
  case h::t if pred(h) => filter2(t,pred)
  case h::t if !pred(h) => h::filter2(t,pred)
}
filter2(list,(a:Int)=>a%2==0)



