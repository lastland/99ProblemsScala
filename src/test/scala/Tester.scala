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
}
