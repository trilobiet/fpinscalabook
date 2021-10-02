package chapter6

import SimpleRNG._

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}


object SimpleRNG {

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type RandFunction[+A] = RNG => (A, RNG)

  val int: RandFunction[Int] = _.nextInt

  def unit[A](a: A): RandFunction[A] = r => (a,r)

  def map[A,B](rf: RandFunction[A])(f: A => B): RandFunction[B] =
    (rng: RNG) => {
      val (a,rng2) = rf(rng)
      (f(a),rng2)
    }

  def nonNegativeEven: RandFunction[Int] =
    map(nonNegativeInt)(i => i - i%2)


  /* ------------------
    Exercise 6.1
  ------------------ */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, rn) = rng.nextInt
    val p = if (r < 0) -r+1 else r
    (p, rn)
  }

  /* ------------------
    Exercise 6.2
  ------------------ */
  def double(rng: RNG): (Double, RNG) = {
    (nonNegativeInt(rng)._1.toDouble / Int.MaxValue, rng)
  }

  /* ------------------
    Exercise 6.3
  ------------------ */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i),rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  /* ------------------
    Exercise 6.4
  ------------------ */
  def ints(count: Int)(rng: RNG): (List[Int],RNG) = {

    @tailrec
    def next(n: Int, rng: RNG, lst: List[Int]): (List[Int],RNG) = {
      if (n > 0) {
        val (i,r) = rng.nextInt
        next(n-1, r, i::lst)
      }
      else (lst,rng)
    }

    next(count,rng,List.empty[Int])
  }


  /* ------------------
    Exercise 6.5
  ------------------ */
  def doubleRf: RandFunction[Double] = {
    // (nonNegativeInt(rng)._1.toDouble / Int.MaxValue, rng)
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue)
  }

  /* ------------------
    Exercise 6.6
  ------------------ */
  def map2[A,B,C](ra: RandFunction[A],rb: RandFunction[B])(f: (A,B) => C): RandFunction[C] =
    (rng: RNG) => {
      val (a,rng2) = ra(rng)
      val (b,rng3) = rb(rng2)
      (f(a,b),rng3)
    }

  def both[A,B](ra: RandFunction[A], rb: RandFunction[B]): RandFunction[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: RandFunction[(Int, Double)] =
    both(int, double)

  val randDoubleInt: RandFunction[(Double, Int)] =
    both(double, int)

  /* ------------------
    Exercise 6.7
    hint: use foldRight and map2
  ------------------ */
  def sequence[A](fs: List[RandFunction[A]]): RandFunction[List[A]] = {

    val lstEmpty = List[A]()
    val zero:RandFunction[List[A]] = rng => (lstEmpty,rng) // is a unit(List[A]())
    fs.foldRight(zero)( (f: RandFunction[A], result: RandFunction[List[A]]) =>
      map2(f, result)( (a:A, b:List[A]) => a::b)
    )
  }

  def intSeq(count: Int): RandFunction[List[Int]] = {

    val ints: List[RandFunction[Int]] = List.fill(count)(int)
    sequence[Int](ints)
  }


  def main(args: Array[String]): Unit = {

    val rng = SimpleRNG(42)
    println(rng) // 42
    val (n1, rng2) = rng.nextInt
    println((n1, rng2)) // must always be exactly (16159453,SimpleRNG(1059025964525))
    val (n2, rng3) = rng2.nextInt
    println((n2, rng3)) // must always be exactly (-1281479697,SimpleRNG(197491923327988))

    val rng4 = SimpleRNG(42)
    val (n3, rng5) = nonNegativeInt(rng4)
    println((n3, rng5)) // must always be exactly (16159453,SimpleRNG(1059025964525))
    val (n4, rng6) = nonNegativeInt(rng5)
    println((n4, rng6)) // must always be exactly (1281479698,SimpleRNG(197491923327988))

    val (n5, rng7) = double(rng5)
    println((n5, rng7)) // must always be exactly (0.5967354861072895,SimpleRNG(1059025964525))

    // Should always print (List(800519575, -930744967, -1427999009, 769882549),SimpleRNG(52462850924773))
    println(ints(4)(SimpleRNG(2001)))


    type RandFunction[+A] = RNG => (Int, RNG)
    def p:RandFunction[Int] = (r:RNG) => r.nextInt

    println(p.apply(SimpleRNG(42)))

    println(doubleRf.apply(SimpleRNG(42)))

    println(randIntDouble.apply(SimpleRNG(42)))
    println(randDoubleInt.apply(SimpleRNG(42)))


    val seq = sequence(List(int,nonNegativeEven)).apply(SimpleRNG(42))
    println(seq)

    // Same as println(ints(4)(SimpleRNG(2001))) though in reverse order...
    println(intSeq(4).apply(SimpleRNG(2001)))

  }

}