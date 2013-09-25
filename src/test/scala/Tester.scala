import org.scalatest.Suite
import Solver.Solver._

class SolverTest extends Suite { 
  // Problem 1
  def testLast() { 
    assert(last(List(1)) === 1)
    assert(last(List(1, 1, 2, 3, 5, 8)) === 8)
    intercept[NoSuchElementException] { 
      last(Nil)
    }
  }
}
