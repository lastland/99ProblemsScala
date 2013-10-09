package S99

object S99BinaryTree { 
  
  // Given code
  sealed abstract class Tree[+T] { 
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def isSymmetric = Tree.isMirrorOf(left, right)
    override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
      if (x < value) Node(value, left.addValue(x), right)
      else Node(value, left, right.addValue(x))
  }
  
  case object End extends Tree[Nothing] {
    override def toString = "."
    override def isSymmetric = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] =
      Node(x)
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

    def isMirrorOf[T](l: Tree[T], r: Tree[T]): Boolean = l match { 
      case Node(_, left, right) => r match { 
	case Node(_, left2, right2) => isMirrorOf(left, right2) && isMirrorOf(right, left2)
	case _ => false
      }
      case End => r == End
      case _ => throw new RuntimeException
    }

    // Problem 57
    def fromList[T <% Ordered[T]](lst: List[T]): Tree[T] =
      lst.foldLeft(End: Tree[T])((t, e) => t.addValue(e))
  }
}
