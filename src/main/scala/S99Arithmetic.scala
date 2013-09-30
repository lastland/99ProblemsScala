package S99
import scala.math._
import scala.collection.immutable.Stream
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

  val primes = 2 #:: Stream.from(3, 2).filter(_ isPrime)
  
  case class S99Int(value: Int) {
    // Problem 31
    def isPrime: Boolean =
      (value > 1) && (primes.takeWhile(_ <= sqrt(value).toInt).forall(value % _ != 0))

    // Problem 33
    def isCoprimeTo(n: Int): Boolean = gcd(value, n) == 1

    // Problem 34
    def totient: Int = range(1, value).filter(isCoprimeTo(_)).length
  }
}
