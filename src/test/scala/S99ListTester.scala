import org.scalatest.Suite
import S99.S99List._

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

  // Problem 15
  def testDuplicateN = { 
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) ===
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    assert(duplicateN(4, List('a)) === List('a, 'a, 'a, 'a))
    assert(duplicateN(0, List('a)) === Nil)
    assert(duplicateN(3, Nil) === Nil)
    intercept[IllegalArgumentException] { 
      duplicateN(-1, List('a))
    }
  }

  // Problem 16
  def testDrop = { 
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  // Problem 17
  def testSplit = { 
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(split(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      (List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i), List('j, 'k)))
    assert(split(4, List('a, 'b, 'c)) === (List('a, 'b, 'c), Nil))
  }

  // Problem 18
  def testSlice = { 
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('d, 'e, 'f, 'g))
    assert(slice(3, 6, List('a, 'b, 'c, 'd)) === List('d))
    assert(slice(3, 6, List('a, 'b, 'c)) === Nil)
    assert(slice(-1, 100, List('a, 'b, 'c)) === List('a, 'b, 'c))
    assert(slice(-1, -2, List('a, 'b, 'c)) === Nil)
  }

  // Problem 19
  def testRotate = { 
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  // Problem 20
  def testRemoveAt = { 
    assert(removeAt(0, List('a, 'b, 'c, 'd)) ===
      (List('b, 'c, 'd), 'a))
    assert(removeAt(1, List('a, 'b, 'c, 'd)) ===
      (List('a, 'c, 'd), 'b))
    intercept[IllegalArgumentException] { 
      removeAt(4, List('a, 'b, 'c, 'd))
    }
  }

  // Problem 21
  def testInsertAt = { 
    assert(insertAt('new, 0, List('a, 'b, 'c, 'd)) ===
      List('new, 'a, 'b, 'c, 'd))
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) ===
      List('a, 'new, 'b, 'c, 'd))
    assert(insertAt('new, 4, List('a, 'b, 'c, 'd)) ===
      List('a, 'b, 'c, 'd, 'new))
  }

  // Problem 22
  def testRange = { 
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
    assert(range(9, 4) === List(4, 5, 6, 7, 8, 9))
    assert(range(5, 5) === List(5))
  }

  // Problem 23. I don't really know how to test random function. = =
  def testRandomSelect = {
    val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
    for (i <- 1 to 20) {
      val r = randomSelect(3, l)
      assert(r.length === 3)
      assert(r.distinct.length === 3)
      for (e <- r) { 
	assert(l.contains(e))
      }
    }
    assert(randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)).length === 3)
  }

  // Problem 24
  def testLotto = { 
    for (i <- 1 to 20) {
      val l = lotto(6, 49)
      assert(l.length === 6)
      assert(l.distinct.length === 6)
      for (e <- l) { 
	assert(e >= 1 && e <= 49)
      }
    }
  }

  // Problem 25
  def testRandomPermute = { 
    for (i <- 1 to 20) { 
      val l = List('a, 'b, 'c, 'd, 'e, 'f)
      val r = randomPermute(l)
      assert(r.length === 6)
      assert(r.distinct.length === 6)
      for (e <- r) { 
	assert(l.contains(e))
      }
    }
  }

  def testCombination = { 
    val l = Array(range(1, 6), range(1, 12))
    val n = Array(3, 3)
    val a = Array(20, 220)
    for (i <- 0 to 1) {
      val r = combination(n(i), l(i))
      assert(r.length === a(i))
      assert(r.distinct.length === a(i))
      for (e <- r) { 
	assert(e.length === n(i))
	assert(e.distinct.length === n(i))
	for (s <- e) { 
	  assert(l(i).contains(s))
	}
      }
    }
  }
}
