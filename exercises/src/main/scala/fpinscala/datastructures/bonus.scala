package fpinscala.datastructures.bonus

/** Tree with arbitrary branching factor at each branch. */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](children: Tree[A]*) extends Tree[A]

object Tree {
  def sizeSimple[A](t: Tree[A]): Int = t match {
    case Branch(ts @ _*) => 1 + (0  /: ts)(_ + size(_))
    case Leaf(_) => 1
  }
  
  def fold[A, B](t: Tree[A])(f: A => B)(g: (Seq[B]) => B): B = t match { //one function per data constructor of ADT
    case Branch(ts @ _*) => g(ts.map(fold(_)(f)(g)))
    case Leaf(a) => f(a)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _.sum)

  def maximum[A : Ordering](t: Tree[A]): A = fold(t)(a => a)(_.max)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _.max) //Leaf has depth 0; Branch has depth 1 + depth of child with max depth (arg to 2nd fn is list of child depths)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_: _*))
}

/** Tree with a value and arbitrary number of children at each branch. */
sealed trait ValueTree[+A] //probably is an official name for this other than Value Tree...
//case class ValueLeaf[A](value: A) extends ValueTree[A] <===== this is just special case of Value with empty child list
case class Value[A](value: A, children: ValueTree[A]*) extends ValueTree[A]

object ValueTree {
  def size1[A](t: ValueTree[A]): Int = t match {
    case Value(_, ts @ _*) if ts.nonEmpty => 1 + (0 /: ts)(_ + size1(_))
    case Value(_) => 1
  }

  def fold[A, B](t: ValueTree[A])(f: A => B)(g: (B, Seq[B]) => B): B = t match {
    //case Value(a, ch @ _*) => g(f(a), ch.map(fold(_)(f)(g)))
    case Value(a, ch @ _*) if ch.nonEmpty => g(f(a), ch.map(fold(_)(f)(g)))
    case Value(a) => f(a)
  }

  def size[A](t: ValueTree[A]): Int = fold(t)(_ => 1)(_ + _.sum)

  def maximum[A : Ordering](t: ValueTree[A]): A = fold(t)(a => a)((b, bs) => implicitly[Ordering[A]].max(b, bs.max))

  def depth[A](t: ValueTree[A]): Int = fold(t)(_ => 0)((_, bs) => 1 + bs.max)

  def map[A, B](t: ValueTree[A])(f: A => B): ValueTree[B] = fold(t)(a => Value(f(a)))((b, bs) => Value(b.value, bs: _*))

  def sum[A : Numeric](t: ValueTree[A]): A = fold(t)(a => a)((b, bs) => implicitly[Numeric[A]].plus(b, bs.sum))
}

sealed trait Graph[+A]
case class Vertex[A](value: A, out: Seq[Vertex[A]]) extends Graph[A]
