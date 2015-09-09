sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1,2,3)
  val total = sum(example)

  def tail1[A](as:A*) = as match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }
  def tail2[A](as:A*) = if(as.isEmpty) Nil else as.drop(1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head,tail) if f(head) =>  dropWhile(tail,f)
    case _ => l
  }

  def setHead[A](l:List[A],newHead:A):List[A] = l match {
    case Nil => sys.error("list is empty!")
    case Cons(head,tail) => Cons(newHead,tail)

  }


}