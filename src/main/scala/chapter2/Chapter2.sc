import scala.annotation.tailrec

/* ------------------
  Exercise 2.1
------------------ */
def fib(x: Long): Long = {
  import scala.annotation.tailrec
  @tailrec
  def calc(n:Long, nMinus1: Long, nMinus2: Long):Long = {
    if (n == 0) nMinus2
    else calc(n-1, nMinus2, nMinus2 + nMinus1)
  }
  calc(x,1,0)
}
// 0 1 1 2 3 5 8 13 21
// 0 1 2 3 4 5 6  7  8
for (i <- 0 to 50) print(s"${fib(i)} ")


/* ------------------
  Exercise 2.2
------------------ */
@tailrec
def isSorted[A] (as: Array[A], ordered:(A,A)=>Boolean): Boolean = {
  if (as.length < 2) true // arrays with one or zero elements are sorted
  else ordered(as.head,as.tail.head) && isSorted(as.tail,ordered)
}

val words1 = Array("jan","piet","klaas")
val words2 = Array("jan","klaas","piet")
val ints1 = Array(3,7,8,100,102,109)
val ints2 = Array(3,7,8,100,109,102)
def orderedString(s1: String, s2: String): Boolean =  s1 < s2
def orderedInt(i1: Int, i2: Int): Boolean =  i1 < i2

println(s"${words1.mkString(", ")} is sorted? ${isSorted[String](words1,orderedString)}")
println(s"${words2.mkString(", ")} is sorted? ${isSorted[String](words2,orderedString)}")
println(s"${ints1.mkString(", ")} is sorted? ${isSorted(ints1,orderedInt)}")
println(s"${ints2.mkString(", ")} is sorted? ${isSorted(ints2,orderedInt)}")


/* ------------------
  Exercise 2.3
------------------ */
def curry[A,B,C] ( f: (A, B) => C ): A => (B => C) = {
  // (a: A) => ((b: B) => f(a,b)) // written out with types
  a => b => f(a,b)
}

def f(a: String, b: Int): Boolean = a.length == b
println(curry(f)("piet")(3))
println(curry(f)("piet")(4))


/* ------------------
  Exercise 2.4
------------------ */
def uncurry[A,B,C] ( f: A => B => C ): (A, B) => C = {

  (a,b) => f(a)(b)

  /* explained:
    (a,b) => {
      val g = f(a)
      g(b)
    }
    so we can say f(a)(b)
  */

}

/* ------------------
  Exercise 2.5
------------------ */
def compose[A,B,C] (f: B => C, g: A => B): A => C = {
  a => f( g(a) )
}