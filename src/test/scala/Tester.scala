import org.scalatest.Suite
import Solver.Solver._

class SolverTest extends Suite { 
  // Problem 1
  def testLast { 
    assert(last(List(1)) === 1)
    assert(last(List(1, 1, 2, 3, 5, 8)) === 8)
    intercept[NoSuchElementException] { 
      last(Nil)
    }
  }

  // Problem 2
  def testPenultimate = {
    assert(penultimate(List(1, 2)) === 1)
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
    intercept[NoSuchElementException] { 
      penultimate(List(1))
    }
    intercept[NoSuchElementException] { 
      penultimate(Nil)
    }
  }

  // Problem 3
  def testNth = {
    val lst = List(1, 2, 3)
    assert(nth(0, lst) === 1)
    assert(nth(1, lst) === 2)
    assert(nth(2, lst) === 3)
    intercept[NoSuchElementException] { 
      nth(-1, lst)
    }
    intercept[NoSuchElementException] { 
      nth(0, Nil)
    }
    intercept[NoSuchElementException] { 
      nth(1, List(1))
    }
  }

  // Problem 4
  def testLength = { 
    assert(length(List(1, 1, 2, 3, 5, 8)) === 6)
    assert(length(List(1)) === 1) 
    assert(length(Nil) === 0)
  }

  // Problem 5
  def testReverse = { 
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
    assert(reverse(List(1)) === List(1))
    assert(reverse(Nil) === Nil)
  }

  // Problem 6
  def testIsPalindrome = { 
    assert(isPalindrome(List(1, 2, 3, 2, 1)) === true)
    assert(isPalindrome(List(1, 1, 2, 3, 5)) === false) 
    assert(isPalindrome(List(1)) === true) 
    assert(isPalindrome(Nil) === true) 
  }

  // Problem 7
  def testFlatten = { 
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
    assert(flatten(List(1, 2, 3, 4)) === List(1, 2, 3, 4))
    assert(flatten(Nil) === Nil)
  }

  // Problem 8
  def testCompress = { 
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress(List('a, 'b, 'c, 'a, 'd, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress(Nil) == Nil)
  }

  // Problem 9
  def testPack = { 
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === 
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(pack(List('a, 'b, 'c)) === List(List('a), List('b), List('c)))
    assert(pack(List('a)) === List(List('a)))
  }

  // Problem 10
  def testEncode = { 
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encode(List('a, 'b, 'c)) === List((1, 'a), (1, 'b), (1, 'c)))
    assert(encode(List('a)) === List((1, 'a)))
  }
  
  // Problem 11
  def testEncodeModified = { 
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    assert(encodeModified(List('a, 'b, 'c)) === List('a, 'b, 'c))
    assert(encodeModified(List('a)) == List('a))
  }

  // Problem 12
  def testDecode = { 
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(decode(List((1, 'a), (0, 'b), (2, 'c))) ===
      List('a, 'c, 'c))
    intercept[IllegalArgumentException] { 
      decode(List((-1, 'a)))
    }
  }

  // Problem 13
  def testEncodeDirect = { 
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encodeDirect(List('a, 'b, 'c)) === List((1, 'a), (1, 'b), (1, 'c)))
    assert(encodeDirect(List('a)) === List((1, 'a)))
  }

  // Problem 14
  def testDuplicate = { 
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) ===
      List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    assert(duplicate(List('a)) === List('a, 'a))
    assert(duplicate(Nil) === Nil)
  }
}
