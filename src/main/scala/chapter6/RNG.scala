package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

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

  }

}