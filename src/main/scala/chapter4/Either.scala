package chapter4

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {

    this flatMap {
      a => {            // A => ...
        b map (         // Either[EE, B]
          x => f(a, x)  // B => Either[EE,C]
        )               // ... Either[EE,C]
      }
    }

  }
}

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case Left(e) :: _ => Left(e)
      case head :: tail => head flatMap (h => sequence(tail) map (t => h :: t))
    }


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case head :: tail => f(head) flatMap (h => traverse(tail)(f) map (t => h :: t))
    }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)


  def main(args: Array[String]): Unit = {
    val l1 = List( Right(1), Right(2), Right(3), Right(4) )
    val l2 = List( Right(1), Right(2), Left("oops"), Right(3), Left("omg"), Right(4) )
    // Should print "Right(List(1, 2, 3, 4))"
    println(sequence(l1))
    // Should print "Left(oops)" (first error encountered)
    println(sequence(l2))
    // Should print "Right(List(1, 2, 3, 4))"
    println(sequence2(l1))
    // Should print "Left(oops)" (first error encountered)
    println(sequence2(l2))
  }

}