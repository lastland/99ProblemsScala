import org.scalatest.Suite
import S99.S99Logic._

class LogicTest extends Suite { 
  // Problem 46
  def testAnd = { 
    assert(and(true, true) === true)
    assert(and(true, false) === false)
    assert(and(false, true) === false)
    assert(and(false, false) === false)
  }

  def testOr = { 
    assert(or(true, true) === true)
    assert(or(true, false) === true)
    assert(or(false, true) === true)
    assert(or(false, false) === false)
  }

  def testNand = { 
    assert(nand(true, true) === false)
    assert(nand(true, false) === true)
    assert(nand(false, true) === true)
    assert(nand(false, false) === true)
  }

  def testNor = { 
    assert(nor(true, true) === false)
    assert(nor(true, false) === false)
    assert(nor(false, true) === false)
    assert(nor(false, false) === true)
  }

  def testXor = { 
    assert(xor(true, true) === false)
    assert(xor(true, false) === true)
    assert(xor(false, true) === true)
    assert(xor(false, false) === false)
  }

  def testImpl = { 
    assert(impl(true, true) === true)
    assert(impl(true, false) === false)
    assert(impl(false, true) === true)
    assert(impl(false, false) === true)
  }

  def testEqu = { 
    assert(equ(true, true) === true)
    assert(equ(true, false) === false)
    assert(equ(false, true) === false)
    assert(equ(false, false) === true)
  }

  def testTable2 = { 
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) { 
      table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    }
    val ans = """|A     B     result
		 |true  true  true 
		 |true  false true 
		 |false true  false
		 |false false false
		 |""".stripMargin
    assert(stream.toString === ans)
  }
}
