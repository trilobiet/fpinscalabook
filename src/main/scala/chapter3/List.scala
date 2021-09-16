package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z:B)(f:(A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns,0)(_+_)

  def product2(ns: List[Double]): Double =
    foldRight(ns,1.0)(_*_)

  /* ------------------
    Exercise 3.1
  ------------------ */
  val x: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4,_))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- this one matches
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // asString for pretty printing
  def asString[A](lst: List[A]) : String =
    lst match {
      case Nil => "[empty]"
      case Cons(x,Nil) => x.toString
      case Cons(x,tail) => s"$x,${asString(tail)}"
    }

  /* ------------------
    Exercise 3.2
  ------------------ */
  def tail[A](lst: List[A]): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  /* ------------------
    Exercise 3.3
  ------------------ */
  def setHead[A](lst: List[A], x:A): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_, t) => Cons(x,t)
    }

  /* ------------------
    Exercise 3.4
  ------------------ */
  @tailrec
  def drop[A](lst: List[A], n: Int): List[A] =
    (lst, n) match {
      case (Nil, _) => Nil
      case (lst, 0) => lst
      case (Cons(_, t), m) => drop(t, m-1)
    }

  /* ------------------
    Exercise 3.5
  ------------------ */
  @tailrec
  def dropWhile[A](lst: List[A], f:A => Boolean): List[A] =
    lst match {
      case Cons(h, t) if f(h) => dropWhile(t,f)
      case _ => lst
    }

  /* ------------------
    Exercise 3.6
  ------------------ */
  def init[A](lst: List[A]): List[A] =
    lst match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  /* ------------------
    Exercise 3.7
  ------------------ */
  // See chapter 5

  /* ------------------
    Exercise 3.8
  ------------------ */
  def passNilAndCons(): List[Int] =
    foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_))

  /* ------------------
    Exercise 3.9
  ------------------ */
  def length[A](as:List[A]): Int =
    foldRight[A,Int](as,0)((_,b) => b + 1 ) // add 1 for each element to count

  /* ------------------
    Exercise 3.10
  ------------------ */
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
    }
  }

  /* ------------------
    Exercise 3.11
  ------------------ */
  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns,0)(_+_)

  def productLeft(ns: List[Double]): Double =
    foldLeft(ns,1.0)(_*_)

  def lengthLeft[A](ns: List[A]): Int =
    foldLeft(ns,0)((x,_) => x+1)

  /* ------------------
    Exercise 3.12
  ------------------ */
  def reverse[A](lst: List[A]): List[A] = {
    val zl = List[A]() // empty (zero) list
    foldLeft(lst,zl)( (list,a:A) => Cons(a,list) )
  }

  /* ------------------
    Exercise 3.13
  ------------------ */
  def foldLeftUsingFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    ??? // dunno
  }

  def foldRightUsingFoldLeft[A,B](as: List[A], z:B)(f:(A,B) => B): B = {
    val asr = reverse(as)
    foldLeft(asr,z)((b,a)=>f(a,b))
  }

  /* ------------------
    Exercise 3.14
  ------------------ */
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    // Old implementation:
    /* a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t,a2))
    } */
    // Now using fold:
    foldRight(a1,a2)((b,lst)=> Cons(b,lst))
  }

  /* ------------------
    Exercise 3.15
  ------------------ */
  // concatenate lists
  def appendListOfLists[A](lists: List[List[A]]): List[A] = {
    val zl = List[A]() // start (zero) element is empty list
    foldLeft(lists,zl)((acc,l) => append(acc,l))
  }

  /* ------------------
    Exercise 3.16
  ------------------ */
  def incOne(ns: List[Int]): List[Int] = {
    val zl = List[Int]()
    // foldLeft(ns, zl)( (rl, a) => append(rl, List(a+1)) ) // will do also
    foldRight(ns, zl)( (n, rl) => Cons(n+1, rl ) )
  }

  /* ------------------
    Exercise 3.17
  ------------------ */
  def doubles2String(ds: List[Double]): List[String] = {
    val zl = List[String]()
    foldRight(ds, zl)( (d,rl) => Cons(d.toString,rl))
  }

  /* ------------------
    Exercise 3.18
  ------------------ */
  // Generalization of 3.16 and 3.17
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    val zl = List[B]() // start with an empty list of B
    foldRight(as, zl)( (a, rl) => Cons(f(a), rl) )
    // Also valid, put zl directly in arguments
    // foldRight(as, List[B]())( (a, rl) => Cons(f(a), rl))
    // Or even a Nil, (with type specified as List[B])
    // foldRight(as, Nil:List[B])( (a, rl) => Cons(f(a), rl))
  }

  /* ------------------
    Exercise 3.19
  ------------------ */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val zl = List[A]() // start with an empty list of A
    foldRight(as, zl)( (a, rl) => if(f(a)) Cons(a, rl) else rl ) // Only add to result if f(a) is true
  }

  /* ------------------
    Exercise 3.20
  ------------------ */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val zl = List[B]() // start with an empty list of B
    foldRight(as, zl)( (a, rl) => append(f(a),rl) )
  }

  /* ------------------
    Exercise 3.21
  ------------------ */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if(f(a)) List(a) else List()) // either a list with 1 or zero elements
    // correct as well:
    // flatMap(as)(a => if(f(a)) List(a) else Nil)
  }

  /* ------------------
    Exercise 3.22
  ------------------ */
  def zip(lst1: List[Int], lst2: List[Int]): List[Int] = {
    (lst1,lst2) match {
      case (l1,l2) if l1 == Nil || l2 == Nil => Nil // shortest list defines length of resulting list
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,zip(t1,t2))
    }
  }

  /* ------------------
    Exercise 3.23
  ------------------ */
  def zipWith[A](lst1: List[A], lst2: List[A])(f: (A,A) => A): List[A] = {
    (lst1, lst2) match {
      case (l1,l2) if l1 == Nil || l2 == Nil => Nil // shortest list defines length of resulting list
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
    }
  }

  /* ------------------
    Exercise 3.24
  ------------------ */
  // TODO matches too much
  def hasSubsequence[A](lst1: List[A], lst2: List[A]): Boolean = {
    (lst1, lst2) match {
      case (Nil,_) => false
      case (_,Nil) => true
      case (Cons(h1,t1),Cons(h2,t2)) =>
        if(h1==h2 && hasSubsequence(t1,t2)) true
        else hasSubsequence(t1,t2)
    }
  }


  def main(args: Array[String]): Unit = {
    println(s"x = $x")
    val list = List(1,2,3,4,5,6,7)
    val list2 = List(1.0,2.0,3.0,4.0,5.0,6.0,7.0)
    val list3 = List(8,9,10)
    val list4 = List(11,12,13)
    val list5 = List(1,2,3,4,5,6,7,8,9,10,11,12)
    val listOfLists = List(list,list3,list4)
    println(s"tail = ${asString(tail(list))}")
    println(s"init = ${asString(init(list))}")
    println(s"setHead(9) = ${asString(setHead(list,9))}")
    println(s"drop(3) = ${asString(drop(list,3))}")
    val f:Int=>Boolean = x => x < 5
    println(s"dropWhile(<5) = ${asString(dropWhile(list,f))}")
    println(s"passNilAndCons = ${asString(passNilAndCons())}")
    println(s"length = ${length(list)}")
    println(s"foldLeft = ${foldLeft(list,0)(_+_)}")

    println(s"sumLeft = ${sumLeft(list)}")
    println(s"productLeft = ${productLeft(list2)}")
    println(s"lengthLeft = ${lengthLeft(list2)}")
    println(s"reverse = ${asString(reverse(list))}")

    println(s"append = ${asString(append(list,list3))}")

    println(s"appendListOfLists = ${asString(appendListOfLists(listOfLists))}")

    println(s"incOne = ${asString(incOne(list))}")
    println(s"doubles2String = ${doubles2String(list2)}")

    println(s"map(x2) = ${asString(map(list)(_*2))}")
    println(s"map(even) = ${asString(map(list)(_%2==0))}")

    println(s"filter(odd) = ${asString(filter(list)(_%2==1))}")

    println(s"flatMap_ii = ${asString(flatMap(list)(i => List(i,i)))}")

    println(s"filter2(odd) = ${asString(filter2(list)(_%2==1))}")

    println(s"zip = ${asString(zip(list,list4))}")
    println(s"zipWith = ${asString(zipWith(list3,list4)(_*_))}")

    println(s"hasSubsequence(list5,list3) = ${hasSubsequence(list5,list3)}")
    println(s"hasSubsequence(list5,list4) = ${hasSubsequence(list5,list4)} <- should be false! TODO")

  }

}
