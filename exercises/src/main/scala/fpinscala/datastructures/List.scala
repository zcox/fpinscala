package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

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

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new IllegalArgumentException("Cannot return tail of empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n == 0) l 
    else if (n < 0) throw new IllegalArgumentException("Cannot drop negative number " + n)
    else l match {
      case Cons(_, t) => drop(t, n-1)
      case Nil => Nil
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => throw new IllegalArgumentException("Empty list has no head")
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => throw new IllegalArgumentException("Empty list has no tail")
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, b) => b+1 }

  /*
  Cons(1, Cons(2, Cons(3, Nil)))
  foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
  foldLeft(Cons(2, Cons(3, Nil)), (0 + 1))(_ + _)
  foldLeft(Cons(3, Nil), ((0 + 1) + 2))(_ + _)
  foldLeft(Nil, ((0 + 1) + 2) + 3)(_ + _)
  ((0 + 1) + 2) + 3
  6
  */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil => z
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0) { (b, _) => b+1 }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) { (r, a) => Cons(a, r) }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}