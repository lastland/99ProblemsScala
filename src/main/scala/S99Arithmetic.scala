package S99

class S99Int(value: Int) {
  import S99Arithmetic._
  
  def isCoprimeTo(n: Int): Boolean = gcd(value, n) == 1
}

object S99Arithmetic { 
  implicit def intToS99Int(x: Int) = new S99Int(x)

  def gcd(a: Int, b: Int): Int = { 
    if (b == 0) a
    else { 
      gcd(b, a % b)
    }
  }
}
