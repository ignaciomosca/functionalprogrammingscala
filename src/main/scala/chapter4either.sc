trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C]
}

case class Right[+A](val get:A) extends Either[Nothing,A]{
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(get))

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f(get,_))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(get)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = b
}

case class Left[+E](val get:E) extends Either[E,Nothing]{
  override def map[B](f: (Nothing) => B): Either[E, B] = this

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
}