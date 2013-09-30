import org.scalatest.Suite
import S99.S99Arithmetic._

class ArithmeticTest extends Suite { 
  // Problem 31
  def testIsPrime = { 
    assert(1.isPrime === false)
    assert(2.isPrime === true)
    assert(3.isPrime === true)
    assert(9.isPrime === false)
    assert(996.isPrime === false)
    assert(997.isPrime === true)
    assert(3569.isPrime === false)
    assert(3571.isPrime === true)
  }
  
  // Problem 32
  def testGcd = { 
    assert(gcd(36, 63) === 9)
    assert(gcd(35, 64) === 1)
  }

  // Problem 33
  def testIsCoprimeTo = { 
    assert(35.isCoprimeTo(64) === true)
    assert(36.isCoprimeTo(64) === false)
  }

  // Problem 34
  def testTotient = { 
    assert(1.totient === 1)
    assert(10.totient === 4)
    assert(36.totient === 12)
    assert(99.totient === 60)
  }
}
