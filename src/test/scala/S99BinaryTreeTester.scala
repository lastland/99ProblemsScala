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
