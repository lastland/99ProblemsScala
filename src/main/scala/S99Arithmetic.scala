package S99
import scala.math._
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
  
  case class S99Int(value: Int) {
    // Problem 33
    def isCoprimeTo(n: Int): Boolean = gcd(value, n) == 1

    // Problem 34
    def totient: Int = range(1, value).filter(_.isCoprimeTo(value)).length
  }
}
