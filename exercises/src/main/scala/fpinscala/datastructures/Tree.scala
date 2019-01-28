package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(a, b) => size(a) + size(b)
  }

  def max(t: Tree[Int]): Int = {
    def innerMax(t: Tree[Int], m: Int = 0): Int = t match {
      case Leaf(x) => x max m
      case Branch(a, b) => innerMax(a, m) max innerMax(b, m)
    }

    innerMax(t)
  }

  def depth[A](t: Tree[A]): Int = {
    def innerDepth(t: Tree[A], d: Int = 0): Int = t match {
      case Leaf(_) => d
      case Branch(a, b) => innerDepth(a, d + 1) max innerDepth(b, d + 1)
    }

    innerDepth(t)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = Tree.fold[A, Int](t)(_ => 1)(_ + _)

  def depth2[A](t: Tree[A]): Int = Tree.fold[A, Int](t)(_ => 1)(1 + _ max _)

  def max2(t: Tree[Int]): Int = Tree.fold[Int, Int](t)(a => a)(_ max _)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = Tree.fold[A, Tree[B]](t)(a => Leaf(f(a)))(Branch(_, _))
}

object TestTree {
  def main(args: Array[String]): Unit = {
    val t = Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))
    val t2 = Branch(Leaf(2), Branch(Leaf(6), Leaf(4)))
    assert(Tree.size(t) == 3)
    assert(Tree.max(t) == 3)
    assert(Tree.depth(t) == 2)
    assert(Tree.map(t)(_ * 2) == t2)
    assert(Tree.size2(t) == 3)
    assert(Tree.depth2(t) == 2)
    assert(Tree.max2(t) == 3)
    assert(Tree.map2(t)(_ * 2) == t2)
  }
}