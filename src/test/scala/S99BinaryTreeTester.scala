import org.scalatest.Suite
import S99.S99BinaryTree._

class BinaryTreeTest extends Suite { 
  // Problem 55
  def testCBalanced = { 
    for (i <- 1 to 10) { 
      val l = Tree.cBalanced(i, "x")
      for (t <- l) {
	assert(isBalanced(t))
      }
    }
  }

  // Problem 56
  def testIsSymmetric = { 
    assert(Node('a', Node('b'), Node('c')).isSymmetric === true)
    assert(Node('a', Node('b'), End).isSymmetric === false)
    assert(Node('a', End, Node('b')).isSymmetric === false)
    assert(Node('a', Node('a', Node('a'), End), Node('a', End, Node('a'))).isSymmetric === true)
    assert(Node('a', Node('a', Node('a'), End), Node('a', Node('a'), End)).isSymmetric === false)
  }

  // Problem 57
  def testAddValueAndFromList = {
    assert(End.addValue(2) === Node(2))
    assert(Node(2).addValue(3) === Node(2, End, Node(3)))
    assert(Node(2, End, Node(3)).addValue(0) === Node(2, Node(0), Node(3)))
    assert(Tree.fromList(List(3, 2, 5, 7, 1)) === Node(3, Node(2, Node(1), End), Node(5, End, Node(7))))
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric === true)
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric === false)
  }

  // Problem 58
  def testSymmetricBalancedTrees = { 
    assert(Tree.symmetricBalancedTrees(5, "x") ===
      List(Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End)),
	   Node("x", Node("x", Node("x"), End), Node("x", End, Node("x")))))
  }

  def isBalanced[T](t: Tree[T]): Boolean = t match { 
    case Node(_, End, End) => true
    case Node(_, l, End) => nodeNum(l) == 1
    case Node(_, End, l) => nodeNum(l) == 1
    case Node(_, l, r) => 
      ((nodeNum(l) - nodeNum(r)).abs <= 1) && isBalanced(l) && isBalanced(r)
    case _ => throw new RuntimeException
  }

  def nodeNum[T](t: Tree[T]): Int = t match { 
    case Node(_, End, End) => 1
    case Node(_, l, End) => 1 + nodeNum(l)
    case Node(_, End, l) => 1 + nodeNum(l)
    case Node(_, l, r) => 1 + nodeNum(l) + nodeNum(r)
    case _ => throw new RuntimeException  // not supposed to happen
  }
}
