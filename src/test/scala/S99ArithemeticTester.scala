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

  // Problem 37
  def testTotienti = { 
    assert(1.totienti === 1)
    assert(10.totienti === 4)
    assert(36.totienti === 12)
    assert(99.totienti === 60)
  }

  // Problem 35
  def testPrimeFactors = {
    assert(1.primeFactors === Nil)
    assert(2.primeFactors === List(2))
    assert(315.primeFactors === List(3, 3, 5, 7))
    assert(3560287.primeFactors === List(997, 3571))
  }

  // Problem 36
  def testPrimeFactorMultiplicity = { 
    assert(1.primeFactorMultiplicity === Map())
    assert(2.primeFactorMultiplicity === Map(2 -> 1))
    assert(315.primeFactorMultiplicity === Map(3 -> 2, 5 -> 1, 7 -> 1))
    assert(991026973.primeFactorMultiplicity === Map(997 -> 3))
  }

  // Problem 38
  def testPerformanceForTotient = { 
    print("totient:  ")
    val a = time(10090.totient)
    print("totienti: ")
    val b = time(10090.totient)
    assert(a === b)
  }

  private def time[A](a: => A) = { 
    val now = System.nanoTime
    val result = a
    val end = (System.nanoTime - now) / 1000
    println("%d microseconds".format(end))
    result
  }

  // Problem 39
  def testListPrimesinRange = { 
    assert(listPrimesinRange(7 to 31) === List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  // Problem 40
  def testGoldbach = { 
    for (i <- 4 to 1000 by 2) { 
      val (x, y) = i.goldbach
      assert(x + y === i)
      assert(x.isPrime)
      assert(y.isPrime)
    }
    intercept[IllegalArgumentException] { 
      2.goldbach
    }
    intercept[IllegalArgumentException] { 
      9.goldbach
    }
  }

  // Problem 41
  def testPrintGoldbachList = { 
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) { 
      printGoldbachList(9 to 20)
    }
    val ans = """|10 = 3 + 7
		 |12 = 5 + 7
		 |14 = 3 + 11
		 |16 = 3 + 13
		 |18 = 5 + 13
		 |20 = 3 + 17
		 |""".stripMargin
    assert(stream.toString === ans)
  }

  def testPrintGoldbachListLimited = { 
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) { 
      printGoldbachListLimited(1 to 2000, 50)
    }
    val ans = """|992 = 73 + 919
		 |1382 = 61 + 1321
		 |1856 = 67 + 1789
		 |1928 = 61 + 1867
		 |""".stripMargin
    assert(stream.toString === ans)
  }
}
