package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    def tolist(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(a, b) => tolist(b(), a() :: l)
      case _ => l
    }
    tolist(this, List.empty[A]) reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((a,b) => p(a) match {
    case true => cons(a, b)
    case false => b
  })

  def forAll(p: A => Boolean): Boolean = (this takeWhile p) == this

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionFoldRight: Option[A] = this.foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B) = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean) = foldRight(empty[A])((a,b) =>
    if (p(a)) cons(a, b) else b
  )

  def append[B >: A](e: => Stream[B]) = foldRight(e)((a,b) => cons(a, b))

  def flatMap[B >: A](f: B => Stream[B]) = foldRight(empty[B])((a,b) => f(a).append(b))

  def mapUnfold[B](f: A => B) = unfold(this){
    case Cons(a,b) => Some((f(a()), b()))
    case _ => None
  }
  def takeUnfold(n: Int) = unfold((this,n)){
    case(Cons(a,b), 1) => Some((a(), (empty, 0)))
    case(Cons(a,b), n) if n > 1 => Some((a(), (b(), n -1)))
    case _ => None
  }
  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(a,b) if p(a()) => Some((a(), b()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fib(a: Int, b: Int): Stream[Int] = cons(a, fib(b, a+b))
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  val fibsUnfold: Stream[Int] = unfold((0,1)){ case(a,b) => Some((a, (b, a+b)))}

  def fromUnfold(a: Int): Stream[Int] = unfold(a)(a => Some((a,a+1)))

  def constantUnfold(a: Int): Stream[Int] = unfold(a)(a => Some((a,a)))

  val onesUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))

  def zipWith[A, B](s: Stream[A], t: Stream[A])(f: (A, A) => B): Stream[B] = unfold((s,t)) {
    case (Cons(a,b), Cons(c,d)) => Some((f(a(), c()), (b(), d())))
    case _ => None
  }

}

object TestStream {
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3)
    val p: Int => Boolean = _ % 2 == 0
    assert(s.toList == List(1,2,3))
    assert(s.take(1).toList == List(1))
    assert(s.takeWhile(p).toList == List.empty[Int])
    assert(s.forAll(_ % 2 == 0) == false)
    assert(s.takeWhileFoldRight(_ % 2 == 1).toList == Stream(1,3).toList)
    assert(s.forAll(p) == false)
    assert(s.headOption == Some(1))
    assert(s.headOptionFoldRight == Some(1))
    assert(s.map(_ + 1).toList == List(2,3,4))
    assert(s.filter(p).toList == List(2))
    assert(s.append(Stream(4)).toList == List(1,2,3,4))
    assert(s.flatMap(b => Stream(b,b)).toList == List(1,1,2,2,3,3))
    assert(Stream.constant(1).take(2).toList == List(1,1))
    assert(Stream.from(1).take(3).toList == s.toList)
    assert(Stream.fibs.take(4).toList == List(0,1,1,2))
    assert(Stream.fibsUnfold.take(4).toList == List(0,1,1,2))
    assert(Stream.fromUnfold(1).take(3).toList == List(1,2,3))
    assert(Stream.constantUnfold(1).take(3).toList == List(1,1,1))
    assert(Stream.onesUnfold.take(3).toList == List(1,1,1))
    assert(s.mapUnfold(_ + 1).toList == List(2,3,4))
    assert(s.takeUnfold(2).toList == List(1,2))
    assert(s.takeWhileUnfold(p).toList == List())
    assert(Stream.zipWith(s,s)(_ + _).toList == List(2,4,6))
  }
}