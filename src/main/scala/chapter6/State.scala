package chapter6

import State._

/* ------------------
  Exercise 6.10
  Generalize RNG unit, map, map2, flatMap and Sequence using State
------------------ */
case class State[S,+A](run: S => (A,S)) {

  def map_explained[B](f: A => B): State[S,B] = {
    val sf: S => (B, S) =
      s => {
        val (a,s2) = run(s) // run is the constructor param S => (A,S)
        val u: (B, S) = (f(a), s2)
        u
      }
    State(sf)
  }

  def map[B](f: A => B): State[S,B] =
    State(s => {
      val (a,s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    State( s => {
      val (a,s2) = run(s)
      val (b,s3) = sb.run(s2)
      (f(a,b),s3)
    })

  def flatMap_explained[B](g: A => State[S,B]): State[S,B] = {

    val sf: S => (B, S) =
      s => {
        val (a,s2) = run(s)
        val sb: State[S, B] = g(a)
        sb.run(s2)
      }
    State(sf)
  }

  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    })

  def mapThroughFlatmap[B](f: A => B): State[S,B] = {
    flatMap(a => State(b => (f(a),b)))
    // which is the same as: flatMap(a => unit(f(a)))
  }

}

object State {

  def unit[A,S](a: A): State[S,A] =
    State(s => (a,s))

  def sequence[A,S](fs: List[State[S,A]]): State[S,List[A]] = {

    val lstEmpty = List[A]()
    val zero: State[S, List[A]] = unit(lstEmpty)
    fs.foldRight(zero)( (f: State[S,A], result: State[S, List[A]]) =>
      f.map2(result)( (a,l) => a::l )
    )
  }

  def main(args: Array[String]): Unit = {

    type Rand[A] = State[RNG, A]
    val s: Rand[Int] = State((rng:RNG) => rng.nextInt)

    val     rng0  = SimpleRNG.SimpleRNG(2)
    val (i1,rng1) = s.run(rng0)
    val (i2,rng2) = s.run(rng1)
    val (i3,_)    = s.run(rng2)

    println( rng0 + "\n  ↓ \n" + i1 + "\n" + rng1 + "\n  ↓ \n" + i2 + "\n" + rng2 + "\n  ↓ \n" + i3 )
    //println( rng0 + " -> " + (i1,rng1) + " -> " + (i2,rng2) + " -> " + (i3)
  }

}

