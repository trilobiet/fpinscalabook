package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /* ------------------
    Exercise 3.25
  ------------------ */
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

  /* ------------------
    Exercise 3.26
  ------------------ */
  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(x) => x
      case Branch(l,r) => maximum(l) max maximum(r)
    }

  /* ------------------
    Exercise 3.27
  ------------------ */
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

  /* ------------------
    Exercise 3.28
  ------------------ */
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(v) => Leaf( f(v) )
      case Branch(l,r) => Branch( map(l)(f), map(r)(f) )
    }

  /* ------------------
    Exercise 3.29
  ------------------ */
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }

  def size2[A](tree: Tree[A]): Int =
    fold[A,Int](tree)(_=>1)(_+_+1)

  def maximum2(tree: Tree[Int]): Int =
    fold[Int,Int](tree)(v => v)(_ max _)

  def depth2[A](tree: Tree[A]): Int = {
    fold[A,Int](tree)(_=>0)( (a1,a2) => 1 + (a1 max a2))
  }

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    // Apparently this will not compile if you don't give the types
    // fold(tree)(a => Leaf(f(a)) )( (t1,t2) => Branch(t1,t2) )
    fold[A,Tree[B]](tree)(a => Leaf(f(a)) )( (t1,t2) => Branch(t1,t2) )
    // OR: fold(tree)(a=>Leaf(f(a)): Tree[B])( Branch(_,_) )
  }

  def asString[A](tree: Tree[A]): String = {
    tree match {
      case Leaf(v) => v.toString
      case Branch(l,r) => asString(l) + " " + asString(r) + "\n"
    }
  }

  def main(args: Array[String]): Unit = {

    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(123), // depth 3
            Leaf(123)
          ),
          Leaf(12)
        ),
        Branch(
          Branch(
            Branch(
              Leaf(1234), // depth 4
              Leaf(1234)
            ),
            Leaf(90002)
          ),
          Branch(
            Branch(
              Leaf(90001),
              Leaf(1234)
            ),
            Branch(
              Leaf(1234),
              Branch(
                Leaf(12345), // depth 5
                Branch(
                  Leaf(123456),
                  Leaf(123456)
                )
              ),
            )
          )
        )
      )

    println(s"size = ${size(tree)}")
    println(s"size2 = ${size2(tree)}")
    println(s"maximum = ${maximum(tree)}")
    println(s"maximum2 = ${maximum2(tree)}")
    println(s"depth = ${depth(tree)}")
    println(s"depth2 = ${depth2(tree)}")
    println(asString(map(tree)(_+1)))
    println(asString(map2(tree)(_+1)))

  }


}
