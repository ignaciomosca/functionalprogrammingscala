/**
 * Exercise 3
 * Write a generic function map2 that combines two Option values
 * using a binary function. If either Option value is None ,
 * then the return value is too.
 * */
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
  for{
    x<-a
    y<-b
  }yield(f(x,y))
}


/**
 * Exercise 4
 * Write a function sequence that combines a list of Option s
 * into one Option containing a list of all the Some values in
 * the original list. If the original list contains None even
 * once, the result of the function should be None ; otherwise
 * the result should be Some with a list of all the values. *
 * */
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case None=>None
  case Nil=>Some(Nil)
  case Some(head)::tail => sequence(tail).map(head::_)
}


def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case head::tail => map2(f(head),traverse(tail)(f))((h,t)=>h::t)
}