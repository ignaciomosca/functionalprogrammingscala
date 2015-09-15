sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
 * Exercise 25
 * Write a function size that counts the number of nodes in a tree.
 * */
def size[A](t:Tree[A]):Int = t match {
  case Leaf(_) => 1
  case Branch(left,right) => 1+size(left)+size(right)
}

/**
 * Exercise 26
 * Write a function maximum that returns the maximum element in a Tree[Int] .
 * */
def maximum(t:Tree[Int]):Int = t match {
  case Leaf(v) => v max maximum(t)
  case Branch(left,right) => maximum(left) max maximum(right)
}

/**
 * Exercise 27
 * Write a function depth that returns the maximum path length
 * from the root of a tree to any leaf.
 * */
def depth[A](t:Tree[A]):Int = t match {
  case Leaf(_) => 0
  case Branch(left,right) => 1 + depth(left) + depth(right)
}


/** Exercise 28
* Write a function map, analogous to the method of the same name on List,
 that modifies each element in a tree with a given function.*/
def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(value) => Leaf(f(value))
  case Branch(left,right) => Branch(map(left)(f),map(right)(f))
}

/**
 * Exercise 29
 * Generalize size , maximum , depth , and map , writing a new
 * function fold that abstracts
 * over their similarities. Reimplement them in terms of this
 * more general function
 * */
def fold2[A,B](t:Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
  case Leaf(v) => f(v)
  case Branch(left,right) => g(fold2(left)(f)(g),fold2(right)(f)(g))
}

def foldSize[A,B](t:Tree[A]):Int = fold2(t)(v=>1)((left,right)=>1 + left + right)

def foldMax[A](t: Tree[Int]): Int = fold2(t)(v=>v)((left,right)=> left max right)

def foldDepth[A](t:Tree[A]):Int = fold2(t)(v=>0)((left,right)=>1+(left max right))

def foldMap[A,B](t:Tree[A])(f:A=>B):Tree[B] = fold2(t)(v=>Leaf(f(v)):Tree[B])((left,right)=>Branch(left,right))
