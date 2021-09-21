package chapter5

sealed trait Stream[+A] {

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
    case Cons(h,t) if n > 0 => Stream.cons( h(), t().take(n-1) )
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this  // we're done
  }

  /* ------------------
     Exercise 5.3
  ------------------ */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons( h(), t().takeWhile(p) )
    case _ => Empty
  }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A]( hd: => A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( ()=>head, ()=>tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons( as.head, apply(as.tail:_*))
  }

  def main(args: Array[String]): Unit = {
    val s1 = Stream.apply("a","b","c","d","e","f","g","h")
    println(s1.toList)
    val t1 = s1.take(4)
    println(t1.toList)
    val d1 = s1.drop(3)
    println(d1.toList)

    val s2 = Stream.apply(5,7,9,8,3,6,11,12)
    val tw = s2.takeWhile(_%2==1) // takeWhile odd
    println(tw.toList)

  }

}
