trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {

  def map[B](f: A => B): Option[B] = Some(f(get))

  def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  def filter(f: (A) => Boolean): Option[A] = if (f(get)) Some(get) else None

  def getOrElse[B >: A](default: => B): B = get

  def orElse[B >: A](ob: => Option[B]): Option[B] = this
}

case object None extends Option[Nothing] {

  def map[B](f: Nothing => B): Option[B] = None

  def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  def filter(f: (Nothing) => Boolean): Option[Nothing] = None

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
}