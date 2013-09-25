package Solver

object Solver {
  // Problem 1
  def last[A](lst: List[A]): A = lst match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }

  // Problem 2
  def penultimate[A](lst: List[A]): A = lst match { 
    case h :: _ :: Nil => h
    case _ :: Nil => throw new NoSuchElementException
    case Nil => throw new NoSuchElementException
    case _ :: t => penultimate(t)
  }

  // Problem 3
  def nth[A](n: Int, lst: List[A]): A = (n, lst) match { 
    case (_, Nil) => throw new NoSuchElementException
    case (0, h :: _) => h
    case (n, _ :: t) if n < 0 => throw new NoSuchElementException
    case (_, _ :: t) => nth(n - 1, t)
  }

  // Problem 4
  def length[A](lst: List[A]): Int = length(lst, 0)

  private def length[A](lst: List[A], res: Int): Int = lst match { 
    case Nil => res
    case _ :: t => length(t, res + 1)
  }

  // Problem 5
  def reverse[A](lst: List[A]): List[A] = reverse(lst, Nil)

  def reverse[A](lst: List[A], res: List[A]): List[A] = lst match { 
    case Nil => res
    case h :: t => reverse(t, h :: res)
  }

  // Problem 6
  def isPalindrome[A](lst: List[A]): Boolean = lst == reverse(lst) // Yes. I'm lazy.

  // Problem 7
  def flatten(lst: List[Any]): List[Any] = lst flatMap { 
    case l: List[_] => flatten(l)
    case elt => List(elt)
  }
}
