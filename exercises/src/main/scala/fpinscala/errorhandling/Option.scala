package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => Some(x)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.head match {
    case None => None
    case Some(x) => a.tail.foldLeft[Option[List[A]]](Some(List(x)))((b, opt) => opt match {
      case None => None
      case Some(o) => b.map(_ ++ List(o))
    })
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = f(a.head) match {
    case None => None
    case Some(x) => a.tail.foldLeft[Option[List[B]]](Some(List(x)))((b, c) => f(c) match {
      case None => None
      case Some(y) => b.map(_ ++ List(y))
    })
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(p => p)
}

object TestOption {
  def main(args: Array[String]): Unit = {
    val t = Seq(1.0,2.0,3.0)
    val v = Some(2.0/3.0)
    assert(Option.variance(t) == v)
    assert(Option.map2(Some(2), Some(4))(_ * _) == Some(8))
    assert(Option.sequence(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(Option.traverse(List(1,2))(Some(_)) == Some(List(1, 2)))
    assert(Option.sequence2(List(Some(1), Some(2))) == Some(List(1,2)))
  }
}