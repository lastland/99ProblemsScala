import org.scalatest.Suite
import S99.S99Arithmetic._
import S99.S99Int

class ArithmeticTest extends Suite { 
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
}
