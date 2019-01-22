package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(List.tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, Nil) => if (f(x)) Nil else l
    case Cons(x, y) => {
      if (f(x)) dropWhile(y)(f)
      else append(List(x), dropWhile(y)(f))
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, y) => List.append(List(x), init(y))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a,b) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, Nil) => f(z,x)
    case Cons(x, y) => foldLeft(y, f(z,x))(f)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, Nil) => f(x, z)
    case Cons(x, y) => foldLeft(List.append(List.reverse(y), List(x)), z)((b, a) => f(a, b))
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2(l: List[_]): Int = foldLeft(l, 0)((z, _) => z + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => l
    case Cons(x, y) => foldLeft(y, List(x))((z, a) => List.append(List(a), z))
  }

  def append2[A](l: List[A], m: List[A]) = foldLeft(reverse(l), m)((b,a) => Cons(a, b))

  def append3[A](ls: List[List[A]]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, y) => foldLeft(y, x)(append(_, _))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = reverse(l) match {
    case Nil => Nil
    case Cons(x, y) => foldLeft(y, List(f(x)))((b,a) => Cons(f(a), b))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldLeft(reverse(l), List[A]())((b,a) => f(a) match {
    case true => Cons(a, b)
    case false => b
  })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldLeft(reverse(as), List[B]())((b,a) => List.append(f(a), b))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((a) => if (f(a)) List(a) else Nil)

  def zipWith[A, B](l: List[A], m: List[A])(f: (A, A) => B): List[B] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a,b), Cons(c,d)) => Cons(f(a,c), zipWith(b,d)(f))
  }

  def
}

object TestList {
  def main(args: Array[String]): Unit = {
    assert(List.tail(List(1)) == List())
    assert(List.tail(List(1,2,3)) == List(2,3))
    assert(List.drop(List(1), 1) == List())
    assert(List.drop(List(1,2,3), 1) == List(2,3))
    assert(List.dropWhile(List(1,2,3))(x => x % 2 == 0) == List(1,3))
    assert(List.init(List(1,2,3)) == List(1,2))
    assert(List.length(List(1,2,3)) == 3)
    assert(List.foldLeft(List(1,2,3), 0)(_ + _) == 6)
    assert(List.sum3(List(1,2,3)) == 6)
    assert(List.product3(List(1,2,3)) == 6)
    assert(List.length2(List(1,2,3)) == 3)
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
    assert(List.foldRight(List("a", "b", "c"), "")(_ + _) == "abc")
    assert(List.foldLeft(List("a", "b", "c"), "")(_ + _) == "abc")
    assert(List.append2(List("a", "b", "c"), List("d", "e", "f")) == List("a", "b", "c", "d", "e", "f"))
    assert(List.append3(List(List("a", "b", "c"), List("d", "e", "f"))) == List("a", "b", "c", "d", "e", "f"))
    assert(List.map(List(1,2,3))(_ + 1) == List(2,3,4))
    assert(List.map(List(1.0, 2.0, 3.0))(_.toString) == List("1.0", "2.0", "3.0"))
    assert(List.filter(List(1,2,3))(_ % 2 == 0) == List(2))
    assert(List.flatMap(List(1,2,3))(p => List(p, p)) == List(1,1,2,2,3,3))
    assert(List.filter2(List(1,2,3))(_ % 2 == 0) == List(2))
    assert(List.zipWith(List(1,2,3), List(4,5,6))(_ * _) == List(4,10,18))


  }
}