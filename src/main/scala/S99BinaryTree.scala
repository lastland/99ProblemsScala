package S99

object S99BinaryTree { 
  
  // Given code
  sealed abstract class Tree[+T] { 
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }
  
  case object End extends Tree[Nothing] {
    override def toString = "."
  }
  
  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree { 
    // Problem 55
    def cBalanced[T](n: Int, elem: T): List[Tree[T]] = { 
      if (n < 1) { 
	List(End)
      } else if (n % 2 == 0) { 
	val l = cBalanced((n - 1) / 2, elem)
	val r = cBalanced((n + 1) / 2, elem)
	l flatMap { i => r flatMap { j => List(Node(elem, i, j), Node(elem, j, i)) } }
      } else { 
	val t = cBalanced((n - 1) / 2, elem)
	t flatMap { i => t map { j => Node(elem, i, j) } } distinct
      }
    }
  }
}
