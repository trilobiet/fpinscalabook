package chapter5

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, =>B) => B): B =
    this match {
      case Cons(h,t) => f( h(), t().foldRight(z)(f) )
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,_) => Some(h())
  }

  /* ------------------
    Exercise 5.1
  ------------------ */
  // Uses standard Scala List, not the List from chapter 3
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  /* ------------------
    Exercise 5.2
  ------------------ */
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons( h(), t().take(n-1) )
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n - 1)
    case _ => this  // we're done
  }

  /* ------------------
     Exercise 5.3
  ------------------ */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons( h(), t().takeWhile(p) )
    case _ => Empty
  }

  /* ------------------
     Exercise 5.4
  ------------------ */
  def forAll(p: A => Boolean): Boolean =
    !exists(!p(_))

  /* ------------------
     Exercise 5.4
  ------------------ */
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight( empty[A] )( (a:A, s) => if(p(a)) cons(a,s) else empty)
    //           ^
    //           | start with an empty stream accumulator


  /* ------------------
     Exercise 5.6
  ------------------ */
  def headOption2: Option[A] = {
    foldRight( Option.empty[A] )( (a, _) => Some(a))
    // zero element can also be None: Option[A]
  }

  /* ------------------
     Exercise 5.7
     Use foldRight
  ------------------ */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,s) => cons(f(a),s))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,s) => f(a).append(s))

  def filter2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,s) => if(f(a)) cons(a,s) else s)

  // Param s is non strict!
  // B must be a supertype of A
  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a,t) => cons(a,t))
    // take the input list as zero element
  }

  /* ------------------
    Exercise 5.13
  ------------------ */
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {  // decomposing function arguments like in fibsUnfold
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),n) if n>0 => Some( (h(), (t(),n-1)) )
      case _ => None
    }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A]( hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( ()=>head, ()=>tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons( as.head, apply(as.tail:_*))
  }

  def ones: Stream[Int] = Stream.cons(1, ones)

  /* ------------------
    Exercise 5.8
  ------------------ */
  def constant[A](a: A): Stream[A] =
    Stream.cons(a,constant(a))

  /* ------------------
    Exercise 5.9
  ------------------ */
  def from[A](n: Int): Stream[Int] =
    Stream.cons(n,from(n+1))

  /* ------------------
    Exercise 5.10
    0 1 1 2 3 5 8
  ------------------ */
  def fibs: Stream[Int] = {
    def fibs(n0: Int, n1: Int): Stream[Int] =
      Stream.cons( n0, fibs(n1, n0+n1) )
    fibs(0,1)
  }

  /* ------------------
    Exercise 5.11
  ------------------ */
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case Some( (a,s) ) => Stream.cons(a, unfold(s)(f) )
      case None => Empty
    }

  /* ------------------
    Exercise 5.12
  ------------------ */
  def fibsUnfold: Stream[Int] =
    unfold[Int,(Int,Int)](0,1){ case(n0,n1) => Some(n0,(n1,n0+n1)) } // see fibs above

  def fromUnfold[A](n: Int): Stream[Int] =
    unfold[Int,Int](n)(x => Some(x,x+1))

  def constantUnfold[A](a: A): Stream[A] =
    unfold[A,A](a)(x => Some(x,x))

  def onesUnfold: Stream[Int] =
    unfold[Int,Int](1)(_ => Some(1,1))


  def main(args: Array[String]): Unit = {
    val s1 = Stream.apply("a","b","c","d","e","f","g","h")
    println(s1.toList)
    val t1 = s1.take(4)
    println(t1.toList)
    val d1 = s1.drop(3)
    println(d1.toList)

    val s2 = Stream.apply(5,7,9,8,3,6,11,12)
    val tw1 = s2.takeWhile(_%2==1) // takeWhile odd
    val tw2 = s2.takeWhile2(_%2==1) // takeWhile odd
    println(tw1.toList)
    println(tw2.toList)

    println(s2.exists(_>9))
    println(s2.exists(_>12))
    println(s2.forAll(_%2==0))
    println(s2.forAll(_<20))

    val s3 = Stream.empty
    println(s1.headOption2)
    println(s2.headOption2)
    println(s3.headOption2)
    val isEven: Int => Boolean = a => a%2==0
    println(s2.map(isEven).toList)

    println(s2.filter2(_%2==0).toList)

    val s4 = Stream.apply(13,14,15)
    println(s2.append(s4).toList)

    println(ones.take(10).toList)
    println(constant(3).take(5).toList)

    println(from(10).take(5).toList)

    println(fibs.take(10).toList)
    println(fibsUnfold.take(10).toList)

    println(fromUnfold(10).take(5).toList)
    println(constantUnfold(3).take(5).toList)
    println(onesUnfold.take(10).toList)

    println(s1.takeUnfold(4).toList)


  }

}
