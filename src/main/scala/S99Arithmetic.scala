package S99
import scala.math._
import scala.collection.immutable.Stream
import scala.collection.mutable.HashSet
import S99List._

object S99Arithmetic { 
  implicit def intToS99Int(x: Int) = new S99Int(x)

  // Problem 32
  def gcd(a: Int, b: Int): Int = { 
    if (b == 0) a
    else { 
      gcd(b, a % b)
    }
  }

  val primeStream = 2 #:: Stream.from(3, 2).filter(_ isPrime)
  val primes = new HashSet[Int]

  private def factWithList(n: Int, lst: Iterable[Int], res: List[Int]): List[Int] = 
    if (n.isPrime) n :: res
    else if (lst.isEmpty || n == 1) res
    else if (n % lst.head == 0) factWithList(n / lst.head, lst, lst.head :: res)
    else factWithList(n, lst.tail, res)
  
  case class S99Int(value: Int) {
    // Problem 31
    def isPrime: Boolean =
      // I want memoize!!!
      if (primes(value))
	true
      else (value > 1) && (primeStream.takeWhile(_ <= sqrt(value).toInt).forall(value % _ != 0))

    // Problem 33
    def isCoprimeTo(n: Int): Boolean = gcd(value, n) == 1

    // Problem 34
    def totient: Int = range(1, value).filter(isCoprimeTo(_)).length

    // Problem 35
    def primeFactors = 
      reverse(factWithList(value, primeStream.takeWhile(_ <= value), Nil))
  }
}
