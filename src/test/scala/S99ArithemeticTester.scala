import org.scalatest.Suite
import S99.S99Arithmetic._

class ArithmeticTest extends Suite { 
  // Problem 32
  def testGcd = { 
    assert(gcd(36, 63) === 9)
    assert(gcd(35, 64) === 1)
  }
}
