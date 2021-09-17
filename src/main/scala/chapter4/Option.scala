package chapter4

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  /* ------------------
    Exercise 4.1
  ------------------ */
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(v) => Some(f(v))
      case _ => None
    }

  def getOrElse[B >: A](default : => B): B =  // lazy
    this match {
      case Some(v) => v
      case _ => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(v) => f(v)
      case _ => None
    }

  def flatMap2[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(_) => this
      case _ => ob
    }

  def orElse2[B >: A](ob: => Option[B]): Option[B] = {
    map(a=>Some(a)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(v) if f(v) => this
      case _ => None
    }

  def filter2(f: A => Boolean): Option[A] =
    ??? // TODO

}

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] =
    x => x map f

  /* ------------------
    Exercise 4.2
  ------------------ */
  def variance(xs: Seq[Double]): Option[Double] = {
    // use flatmap
    // variance x = math.pow(x-m, 2) for each x
    val mn: Option[Double] = mean(xs);
    mn.flatMap( m => mean(xs.map(x => math.pow(x-m,2))) )
  }

  /* ------------------
    Exercise 4.3
    (Use flatMap)
  ------------------ */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap( a1 => b.map( b1 => f(a1, b1)))

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A,B,C) => D): Option[D] =
    a.flatMap( a1 => b.flatMap( b1 => c.map( c1 => f(a1, b1, c1))))

  def map4[A,B,C,D,E](a: Option[A], b: Option[B], c: Option[C], d: Option[D])(f: (A,B,C,D) => E): Option[E] =
    a.flatMap( a1 => b.flatMap( b1 => c.flatMap( c1 => d.map( d1 => f(a1, b1, c1, d1)))))

  /* ------------------
    Exercise 4.4
  ------------------ */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)  // end of list must not break the chain of 'Some's!
      case head :: tail => head.flatMap( h => sequence(tail).map(t => h::t) )
    }

  /* ------------------
    Exercise 4.5
  ------------------ */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match {
      case Nil => Some(Nil) // end of list must not break the chain of 'Some's!
      case head :: tail => f(head).flatMap( h => traverse(tail)(f).map(t => h::t) )
    }

  def seq2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)



  def main(args: Array[String]): Unit = {
    val some = Some[String]("Hallo")
    val none = None

    println(some.orElse(Some[String]("Hoort u mij")))
    println(none.orElse(Some[String]("Hoort u mij")))
    println()
    println(some.orElse2(Some[String]("Hoort u mij")))
    println(none.orElse2(Some[String]("Hoort u mij")))

    val lst1 = List( Some("cat"), Some("dog"), Some("bird") )
    val lst2 = List( Some("cat"), Some("dog"), None, Some("bird") )
    // should print "Some(List(cat, dog, bird))"
    println(sequence(lst1))
    // should print "None"
    println(sequence(lst2))
    // should print "Some(List(cat, dog, bird))"
    println(seq2(lst1))
    // should print "None"
    println(seq2(lst2))

  }

}



