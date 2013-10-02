package S99

object S99Logic { 
  implicit def booleanToS99Bool(x: Boolean) = new S99Bool(x)
  
  // Problem 46
  def not(a: Boolean): Boolean = a match { 
    case true => false
    case false => true
  }

  def and(a: Boolean, b: Boolean): Boolean = (a, b) match { 
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match { 
    case (false, false) => false
    case _ => true
  }

  def impl(a: Boolean, b: Boolean): Boolean = (a, b) match { 
    case (true, false) => false
    case _ => true
  }

  // Here I try to define and use all basic operations of my own,
  // but the sarcasm is that the pattern matching actually never use function 'equ'.
  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def xor(a: Boolean, b: Boolean): Boolean = (a, b) match { 
    case (x, y) if x == y => false
    case _ => true
  }

  def table2(f: (Boolean, Boolean) => Boolean) { 
    println("A     B     result")
    for (i <- List(true, false))
      for (j <- List(true, false))
	println("%-5s %-5s %-5s".format(i, j, f(i, j)))
  }
  
  // Problem 47
  class S99Bool(value: Boolean) { 
    def and(x: Boolean): Boolean = S99Logic.and(value, x)
    def or(x: Boolean): Boolean = S99Logic.or(value, x)
    def nand(x: Boolean): Boolean = S99Logic.nand(value, x)
    def nor(x: Boolean): Boolean = S99Logic.nor(value, x)
    def xor(x: Boolean): Boolean = S99Logic.xor(value, x)
    def impl(x: Boolean): Boolean = S99Logic.impl(value, x)
    def equ(x: Boolean): Boolean = S99Logic.equ(value, x)
  }
}
